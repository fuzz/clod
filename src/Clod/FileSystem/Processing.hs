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
--
-- The implementation uses Kleisli arrows for elegant function composition.

module Clod.FileSystem.Processing
  ( -- * File processing
    processFiles
  , ManifestEntry(..)
  
    -- * Manifest operations
  , createOptimizedName
  , writeManifestFile
  , escapeJSON
  ) where

import qualified Data.List as L
import Data.List (nubBy)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified System.IO

import qualified System.Directory as D (copyFile)

import Clod.Types (OptimizedName(..), OriginalPath(..), ClodM, ClodConfig(..), liftIO)
import Clod.IgnorePatterns (matchesIgnorePattern)
import Clod.FileSystem.Detection (isTextFile)
import Clod.FileSystem.Transformations (transformFilename)

-- | A manifest entry consisting of an optimized name and original path
data ManifestEntry = ManifestEntry 
  { entryOptimizedName :: OptimizedName
  , entryOriginalPath :: OriginalPath
  } deriving (Show, Eq)

-- | Read any existing entries from a manifest file
readManifestEntries :: FilePath -> ClodM [ManifestEntry]
readManifestEntries manifestPath = do
  -- Check if manifest exists
  exists <- liftIO $ doesFileExist manifestPath
  if not exists
    then return []
    else do
      -- Read the content (using strict IO to ensure file handle is closed)
      content <- liftIO $ do
        fileContent <- readFile manifestPath
        length fileContent `seq` return fileContent
      
      -- Simple parsing to extract entries (basic approach)
      let parseEntry line =
            case break (== ':') line of
              (optimizedPart, ':':restPart) -> do
                let chars :: String
                    chars = " \"," 
                    cleanOptimized = filter (\c -> not (c `elem` chars)) optimizedPart
                    cleanOriginal = filter (\c -> not (c `elem` chars)) restPart
                if null cleanOptimized || null cleanOriginal
                  then Nothing
                  else Just $ ManifestEntry 
                           (OptimizedName cleanOptimized) 
                           (OriginalPath cleanOriginal)
              _ -> Nothing
              
      -- Split by lines and parse each entry line
      let possibleEntries = filter (\l -> ":" `L.isInfixOf` l) (lines content)
          entries = mapMaybe parseEntry possibleEntries
      
      return entries
      
      where
        mapMaybe f = map fromJust . filter isJust . map f
        isJust (Just _) = True
        isJust Nothing = False
        fromJust (Just x) = x
        fromJust Nothing = error "Impossible: fromJust Nothing"

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
  -- First collect all valid entries
  fileResults <- mapM processOneFile files
  
  -- Extract successful entries and count results
  let newEntries = concatMap (maybe [] id . fst) fileResults
      processed = length newEntries
      skipped = sum $ map snd fileResults
  
  -- Read any existing entries from the manifest, if it exists
  existingEntries <- readManifestEntries manifestPath
  
  -- Combine existing and new entries, then deduplicate
  let allEntries = existingEntries ++ newEntries
      -- Use original path as deduplication key
      uniqueEntries = nubBy (\a b -> entryOriginalPath a == entryOriginalPath b) allEntries
  
  -- Write all entries to the manifest file at once
  writeManifestFile manifestPath uniqueEntries
  
  -- Return file counts
  return (processed, skipped)
  where
    -- Process a single file and return Maybe (entries, skipped count)
    processOneFile :: FilePath -> ClodM (Maybe [ManifestEntry], Int)
    processOneFile file = do
      -- Get full path
      let fullPath = projectPath config </> file
      
      -- Skip if not a regular file
      isFile <- liftIO $ doesFileExist fullPath
      if not isFile
        then return (Nothing, 0)
        else do
          -- Skip any files in the staging directory
          if stagingDir config `L.isInfixOf` fullPath
            then do
              liftIO $ putStrLn $ "Skipping: " ++ fullPath ++ " (in staging directory)"
              return (Nothing, 0)
            else do
              -- Process the file based on manifest-only flag
              if includeInManifestOnly
                then processForManifestOnly config fullPath file
                else processWithCopy config fullPath file
    
    -- Process for manifest only (no copying)
    processForManifestOnly :: ClodConfig -> FilePath -> FilePath -> ClodM (Maybe [ManifestEntry], Int)
    processForManifestOnly cfg fullPath relPath = do
      -- Skip if matches ignore patterns
      let patterns = ignorePatterns cfg
      if not (null patterns) && matchesIgnorePattern patterns relPath
        then return (Nothing, 1)
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return (Nothing, 1)
            else do
              -- Create the manifest entry
              let optimizedName = createOptimizedName relPath
                  originalPath = OriginalPath relPath
                  entry = ManifestEntry optimizedName originalPath
              return (Just [entry], 0)
    
    -- Process with file copying
    processWithCopy :: ClodConfig -> FilePath -> FilePath -> ClodM (Maybe [ManifestEntry], Int)
    processWithCopy cfg fullPath relPath
      -- Skip specifically excluded files
      | relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"] = 
          return (Nothing, 1)
      | otherwise = do
          -- Skip if matches ignore patterns
          let patterns = ignorePatterns cfg
          if not (null patterns) && matchesIgnorePattern patterns relPath
            then return (Nothing, 1)
            else do
              -- Skip binary files
              isText <- isTextFile fullPath
              if not isText
                then return (Nothing, 1)
                else do
                  -- Create the manifest entry
                  let optimizedName = createOptimizedName relPath
                      originalPath = OriginalPath relPath
                      entry = ManifestEntry optimizedName originalPath
                      -- Helper function to extract the name from OptimizedName
                      getOptimizedName (OptimizedName name) = name
                  
                  -- Copy file with optimized name
                  liftIO $ D.copyFile fullPath (currentStaging cfg </> getOptimizedName optimizedName)
                  
                  -- Report the copy operation
                  liftIO $ putStrLn $ "Copied: " ++ relPath ++ " → " ++ getOptimizedName optimizedName
                  
                  return (Just [entry], 0)

-- | Write all entries to the manifest file at once
writeManifestFile :: FilePath -> [ManifestEntry] -> ClodM ()
writeManifestFile manifestPath entries = do
  -- Create the manifest content with all entries
  let manifestLines = "{\n" : entryLines ++ ["\n}"]
      entryLines = zipWith formatEntry [0..] entries
      
      -- Format a single entry (with comma for all but the last)
      formatEntry idx entry =
        let comma = if idx == length entries - 1 then "" else ","
            OptimizedName optimizedName = entryOptimizedName entry
            OriginalPath originalPath = entryOriginalPath entry
            escapedOptimizedName = escapeJSON optimizedName
            escapedOriginalPath = escapeJSON originalPath
        in "  \"" ++ escapedOptimizedName ++ "\": \"" ++ escapedOriginalPath ++ "\"" ++ comma
  
  -- Write the complete manifest file at once, ensuring handles are closed promptly
  content <- return $ unlines manifestLines
  liftIO $ System.IO.withFile manifestPath System.IO.WriteMode $ \h -> do
    System.IO.hPutStr h content
    System.IO.hFlush h

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

-- | Escape JSON special characters 
escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]