{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.Processing
-- Description : File processing operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for processing files, including
-- adding files to the manifest and copying them to the staging directory.

module Clod.FileSystem.Processing
  ( -- * File processing
    processFiles
  , processFile
  , processFileManifestOnly
  
    -- * Manifest operations
  , createOptimizedName
  , addToManifest
  , escapeJSON
  ) where

import Control.Monad (forM, unless)
import qualified Data.List as L
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

import qualified System.Directory as D (copyFile)

import Clod.Types (OptimizedName(..), OriginalPath(..), ClodM, ClodConfig(..), FileResult(..), liftIO)
import Clod.IgnorePatterns (matchesIgnorePattern)
import Clod.FileSystem.Detection (isTextFile)
import Clod.FileSystem.Transformations (transformFilename)

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
              Skipped _ -> do
                -- Skip the verbose output unless we add a verbose flag later
                return (0, 1)
  
  -- Sum the file and skipped counts
  return (sum (map fst results), sum (map snd results))

-- | Create an optimized filename for Claude UI
createOptimizedName :: FilePath -> OptimizedName
createOptimizedName relPath = OptimizedName finalOptimizedName
  where
    dirPart = takeDirectory relPath
    fileName = takeFileName relPath
    
    -- Create the optimized name by replacing slashes with dashes
    optimizedName = case dirPart of
      "." -> fileName
      _   -> map (\c -> if c == '/' then '-' else c) dirPart ++ "-" ++ fileName
    
    -- Apply any special transformations needed for Claude compatibility
    finalOptimizedName = transformFilename optimizedName fileName

-- | Add entry to path manifest
addToManifest :: FilePath -> OriginalPath -> OptimizedName -> IORef Bool -> ClodM ()
addToManifest manifestPath (OriginalPath relPath) (OptimizedName optimizedName) firstEntryRef = do
  -- Use firstEntryRef to track whether a comma is needed
  isFirst <- liftIO $ readIORef firstEntryRef
  unless isFirst $
    liftIO $ appendFile manifestPath ",\n"
  liftIO $ writeIORef firstEntryRef False
  
  -- Escape JSON special characters
  let escapedOptimizedName = escapeJSON optimizedName
      escapedRelPath = escapeJSON relPath
      manifestEntry = "  \"" ++ escapedOptimizedName ++ "\": \"" ++ escapedRelPath ++ "\""
  
  liftIO $ appendFile manifestPath manifestEntry

-- | Process a single file for manifest only (no file copying)
processFileManifestOnly :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> ClodM FileResult
processFileManifestOnly config manifestPath fullPath relPath firstEntryRef = do
  -- Check if file should be ignored according to ignore patterns
  let patterns = ignorePatterns config
      
  if not (null patterns) && matchesIgnorePattern patterns relPath
    then return $ Skipped "matched .clodignore pattern"
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return $ Skipped "binary file"
            else do
              -- Create optimized filename
              let finalOptimizedName = createOptimizedName relPath
                  originalPath = OriginalPath relPath
              
              -- Add to path manifest
              addToManifest manifestPath originalPath finalOptimizedName firstEntryRef
              
              return Success

-- | Process a single file
processFile :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> ClodM FileResult
processFile config manifestPath fullPath relPath firstEntryRef 
  -- Skip specifically excluded files
  | relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"] = 
      return $ Skipped "excluded file"
  | otherwise = do
      -- Check if file should be ignored according to ignore patterns
      let patterns = ignorePatterns config
      
      if not (null patterns) && matchesIgnorePattern patterns relPath
        then return $ Skipped "matched .clodignore pattern"
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return $ Skipped "binary file"
            else do
              -- Create optimized filename
              let finalOptimizedName = createOptimizedName relPath
                  originalPath = OriginalPath relPath
              
              -- Copy file with optimized name
              liftIO $ D.copyFile fullPath (currentStaging config </> unOptimizedName finalOptimizedName)
              
              -- Add to path manifest
              addToManifest manifestPath originalPath finalOptimizedName firstEntryRef
              
              liftIO $ putStrLn $ "Copied: " ++ relPath ++ " → " ++ unOptimizedName finalOptimizedName
              return Success

-- | Escape JSON special characters 
escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]