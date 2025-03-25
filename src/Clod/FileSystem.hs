{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem
-- Description : File system operations for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides functionality for working with files and directories,
-- including finding, reading, copying, and checking files.
--
-- The module handles various file system tasks:
--
-- * Recursively finding files in a directory structure
-- * Detecting modified files since a given timestamp
-- * Identifying text vs. binary files
-- * Processing files according to ignore patterns
-- * Creating an optimized file structure for Claude AI integration
--
-- === File Processing Pipeline
--
-- 1. Files are discovered recursively in the repository
-- 2. Each file is checked against .gitignore and .clodignore patterns
-- 3. Binary files are excluded
-- 4. Remaining files are copied to a staging directory with optimized names
-- 5. A path manifest is created to map optimized names back to original paths
--
-- === Optimized Naming
--
-- Files are renamed for Claude's UI by:
--
-- * Replacing directory separators with dashes
-- * Flattening the directory structure
-- * Special handling for certain file types (e.g., .svg files become .xml)
--
-- This ensures that all files can be easily distinguished in Claude's UI
-- while maintaining a mapping back to their original locations.

module Clod.FileSystem
  ( -- * File finding and filtering
    findAllFiles
  , isModifiedSince
  , isTextFile
  , checkByExtension
  
    -- * File content operations  
  , processFiles
  , processFile
  , copyFile
  , safeRemoveFile
  
    -- * Path and filename handling
  , escapeJSON
  ) where

import Control.Exception (try)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (UTCTime)
import System.Directory
import System.FilePath
import System.IO.Error (catchIOError, IOError)
import System.Process (readProcess)

import Clod.Types
import Clod.IgnorePatterns (matchesIgnorePattern)

-- | Recursively find all files in a directory
--
-- This function takes a base path and a list of files/directories,
-- and recursively finds all files within those directories.
-- It returns paths relative to the base path.
--
-- @
-- -- Find all files in the "src" directory
-- files <- findAllFiles "/path/to/repo" ["src"]
--
-- -- Find all files in multiple directories
-- files <- findAllFiles "/path/to/repo" ["src", "docs", "tests"]
-- @
findAllFiles :: FilePath -> [FilePath] -> ClodM [FilePath]
findAllFiles basePath files = do
  -- Process each entry
  fileResults <- forM files $ \file -> do
    let fullPath = basePath </> file
    isDir <- liftIO $ doesDirectoryExist fullPath
    
    if isDir
      then do
        -- If it's a directory, get its contents and recurse
        contents <- liftIO $ getDirectoryContents fullPath
        let validContents = filter (\f -> not (f `elem` [".", ".."])) contents
        subFiles <- findAllFiles fullPath validContents
        -- Return subdirectory files with their paths relative to the project root
        return $ map (\f -> file </> f) subFiles
      else do
        -- If it's a file, return it
        return [file]
        
  -- Flatten the list of lists
  return $ concat fileResults

-- | Check if a file has been modified since the given time
isModifiedSince :: FilePath -> UTCTime -> FilePath -> ClodM Bool
isModifiedSince basePath lastRunTime relPath = do
  let fullPath = basePath </> relPath
  fileExists <- liftIO $ doesFileExist fullPath
  if not fileExists
    then return False
    else do
      modTime <- liftIO $ getModificationTime fullPath
      return (modTime > lastRunTime)

-- | Check if a file is a text file using the 'file' command or extension
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  -- Use 'file' command to check mime type
  result <- liftIO $ try (readProcess "file" ["--mime-type", "-b", file] "") :: ClodM (Either IOError String)
  case result of
    Left _ -> checkByExtension file  -- If command fails, fall back to extension
    Right mimeType -> 
      if "text/" `L.isPrefixOf` mimeType
        then return True
        else checkByExtension file  -- If not text mime type, check extension

-- | Check if a file is likely a text file based on its extension
checkByExtension :: FilePath -> ClodM Bool
checkByExtension file = do
  let ext = takeExtension file
      textExtensions = [".md", ".txt", ".js", ".jsx", ".ts", ".tsx", ".html", ".css", 
                     ".scss", ".json", ".yaml", ".yml", ".xml", ".svg", ".sh", ".py", 
                     ".rb", ".php", ".hs", ".cabal", ".h", ".c", ".cpp", ".java"]
  return $ ext `elem` textExtensions

-- | Process a list of files for Claude integration
--
-- This is the core function that processes files for Claude integration.
-- It filters files based on ignore patterns, skips binary files, and
-- either copies the files to the staging directory or just adds them to the manifest.
--
-- The function returns a tuple with:
-- 
-- * The number of files successfully processed
-- * The number of files skipped
--
-- @
-- -- Process all files in a list
-- (processed, skipped) <- processFiles config manifestPath allFiles False
--
-- -- Process files but only include in manifest (no copying)
-- (processed, skipped) <- processFiles config manifestPath allFiles True
-- @
processFiles :: ClodConfig    -- ^ Configuration for the Clod program
             -> FilePath      -- ^ Path to the manifest file
             -> [FilePath]    -- ^ List of files to process
             -> Bool          -- ^ Whether to only include in manifest (no file copying)
             -> ClodM (Int, Int)  -- ^ (Processed count, Skipped count)
processFiles config manifestPath files includeInManifestOnly = do
  -- Track if the current entry is the first in the manifest
  ref <- liftIO $ newIORef True
  
  -- Process files and count results
  results <- forM files $ \file -> do
    -- Get full path
    let fullPath = projectPath config </> file
    
    -- Skip if not a regular file
    isFile <- liftIO $ doesFileExist fullPath
    if not isFile
      then return (0, 0)
      else do
        -- Skip any files in the staging directory
        if stagingDir config `L.isInfixOf` fullPath
          then do
            liftIO $ putStrLn $ "Skipping: " ++ fullPath ++ " (in staging directory)"
            return (0, 0)
          else do
            -- Process the file normally (always copy, we're simplifying the logic)
            result <- if includeInManifestOnly
                     then processFileManifestOnly config manifestPath fullPath file ref
                     else processFile config manifestPath fullPath file ref
            case result of
              Success -> return (1, 0)
              Skipped reason -> do
                -- Skip the verbose output unless we add a verbose flag later
                return (0, 1)
  
  -- Sum the file and skipped counts
  return (sum (map fst results), sum (map snd results))

-- | Process a single file for manifest only (no file copying)
processFileManifestOnly :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> ClodM FileResult
processFileManifestOnly config manifestPath fullPath relPath firstEntryRef = do
  -- Skip specifically excluded files
  if relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"]
    then return $ Skipped "excluded file"
    else do
      -- Check if file should be ignored according to ignore patterns
      let ignorePatterns' = ignorePatterns config
      
      if not (null ignorePatterns') && matchesIgnorePattern ignorePatterns' relPath
        then return $ Skipped "matched .clodignore pattern"
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return $ Skipped "binary file"
            else do
              -- Create optimized filename
              let dirPart = takeDirectory relPath
                  fileName = takeFileName relPath
                  
                  -- Create the optimized name by replacing slashes with dashes
                  optimizedName = if dirPart /= "."
                                  then map (\c -> if c == '/' then '-' else c) dirPart ++ "-" ++ fileName
                                  else fileName
                  
                  -- Handle SVG files specially - change to XML extension for Claude compatibility
                  finalOptimizedName = if ".svg" `L.isSuffixOf` fileName
                                      then take (length optimizedName - 4) optimizedName ++ "-svg.xml"
                                      else optimizedName
              
              -- Add to path manifest - use firstEntryRef to track whether a comma is needed
              isFirst <- liftIO $ readIORef firstEntryRef
              unless isFirst $
                liftIO $ appendFile manifestPath ",\n"
              liftIO $ writeIORef firstEntryRef False
              
              -- Escape JSON special characters
              let escapedOptimizedName = escapeJSON finalOptimizedName
                  escapedRelPath = escapeJSON relPath
                  manifestEntry = "  \"" ++ escapedOptimizedName ++ "\": \"" ++ escapedRelPath ++ "\""
              
              liftIO $ appendFile manifestPath manifestEntry
              
              return Success

-- | Process a single file
processFile :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> ClodM FileResult
processFile config manifestPath fullPath relPath firstEntryRef = do
  -- Skip specifically excluded files
  if relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"]
    then return $ Skipped "excluded file"
    else do
      -- Check if file should be ignored according to ignore patterns
      let ignorePatterns' = ignorePatterns config
      
      if not (null ignorePatterns') && matchesIgnorePattern ignorePatterns' relPath
        then return $ Skipped "matched .clodignore pattern"
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return $ Skipped "binary file"
            else do
              -- Create optimized filename
              let dirPart = takeDirectory relPath
                  fileName = takeFileName relPath
                  
                  -- Create the optimized name by replacing slashes with dashes
                  optimizedName = if dirPart /= "."
                                  then map (\c -> if c == '/' then '-' else c) dirPart ++ "-" ++ fileName
                                  else fileName
                  
                  -- Handle SVG files specially - change to XML extension for Claude compatibility
                  finalOptimizedName = if ".svg" `L.isSuffixOf` fileName
                                      then take (length optimizedName - 4) optimizedName ++ "-svg.xml"
                                      else optimizedName
              
              -- Copy file with optimized name
              liftIO $ copyFile fullPath (currentStaging config </> finalOptimizedName)
              
              -- Add to path manifest - use firstEntryRef to track whether a comma is needed
              isFirst <- liftIO $ readIORef firstEntryRef
              unless isFirst $
                liftIO $ appendFile manifestPath ",\n"
              liftIO $ writeIORef firstEntryRef False
              
              -- Escape JSON special characters
              let escapedOptimizedName = escapeJSON finalOptimizedName
                  escapedRelPath = escapeJSON relPath
                  manifestEntry = "  \"" ++ escapedOptimizedName ++ "\": \"" ++ escapedRelPath ++ "\""
              
              liftIO $ appendFile manifestPath manifestEntry
              
              liftIO $ putStrLn $ "Copied: " ++ relPath ++ " → " ++ finalOptimizedName
              return Success

-- | Safely remove a file, ignoring errors if it doesn't exist
safeRemoveFile :: FilePath -> ClodM ()
safeRemoveFile path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

-- | Escape JSON special characters 
escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]