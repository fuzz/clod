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
  ) where

import qualified Data.List as L
import Data.List (nubBy)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified System.IO
import System.IO (stderr, hPutStrLn)
import Control.Monad (when)
import Control.Monad.Except (throwError)

import qualified System.Directory as D (copyFile)

import Clod.Types (OptimizedName(..), OriginalPath(..), ClodM, ClodConfig(..), 
                  liftIO, FileWriteCap(..), fileWriteCap, isPathAllowed, ClodError(..))
import Clod.IgnorePatterns (matchesIgnorePattern)
import Clod.FileSystem.Detection (isTextFile)
import Clod.FileSystem.Transformations (transformFilename)

-- | A manifest entry consisting of an optimized name and original path
--
-- Each entry in the path manifest maps an optimized file name (as displayed in Claude) 
-- to its original file path in the repository.
--
-- @
-- ManifestEntry 
--   { entryOptimizedName = OptimizedName "src-config-settings.js"
--   , entryOriginalPath = OriginalPath "src/config/settings.js"
--   }
-- @
data ManifestEntry = ManifestEntry 
  { entryOptimizedName :: OptimizedName  -- ^ The optimized filename shown in Claude's UI
  , entryOriginalPath :: OriginalPath   -- ^ The original path in the repository
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
      
      -- Parse Dhall record format
      -- This parsing handles the record format with keys and string values
      let stripQuotes s = case s of
            '"':rest -> case reverse rest of
              '"':revRest -> reverse revRest
              _ -> s
            _ -> s
            
          -- Strip backticks from keys if present
          stripBackticks s = case s of
            '`':rest -> case reverse rest of
              '`':revRest -> reverse revRest
              _ -> s
            _ -> s
      
          -- Parse Dhall key-value pairs
          parseEntry line = do
            -- Look for the = sign that separates key and value
            let parts = break (== '=') line
            case parts of
              (keyPart, '=':valuePart) -> do
                let key = strip (stripBackticks (strip keyPart))
                    value = strip (stripQuotes (strip valuePart))
                -- Filter out any entry where key or value is empty
                if null key || null value
                  then Nothing
                  else Just $ ManifestEntry 
                         (OptimizedName key) 
                         (OriginalPath value)
              _ -> Nothing
              
      -- Get lines that look like Dhall record entries
      let possibleEntries = filter (\l -> "=" `L.isInfixOf` l) (lines content)
          entries = mapMaybe parseEntry possibleEntries
      
      return entries
      
      where
        mapMaybe f = map fromJust . filter isJust . map f
        isJust (Just _) = True
        isJust Nothing = False
        fromJust (Just x) = x
        fromJust Nothing = error "Impossible: fromJust Nothing"
        
        -- Remove leading and trailing whitespace
        strip :: String -> String
        strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        
        -- Check if a character is whitespace
        isSpace :: Char -> Bool
        isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

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
  
  -- Create a FileWriteCap for the staging directory
  let writeCap = fileWriteCap [takeDirectory manifestPath]
  
  -- Write all entries to the manifest file at once
  let manifestPairs = map (\e -> (entryOptimizedName e, entryOriginalPath e)) uniqueEntries
  writeManifestFile writeCap manifestPath manifestPairs
  
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
              when (verbose config) $ do
                liftIO $ hPutStrLn stderr $ "Skipping: " ++ fullPath ++ " (in staging directory)"
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
      -- Skip specifically excluded files and directories
      | relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"] = 
          return (Nothing, 1)
      -- Skip any paths containing node_modules or .git directories
      | "node_modules" `L.isInfixOf` relPath || ".git/" `L.isPrefixOf` relPath || ".git" == relPath =
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
                  
                  -- Report the copy operation only when verbose flag is set (to stderr)
                  when (verbose cfg) $ do
                    liftIO $ hPutStrLn stderr $ "Copied: " ++ relPath ++ " → " ++ getOptimizedName optimizedName
                  
                  return (Just [entry], 0)

-- | Write all entries to the manifest file at once
--
-- This creates the _path_manifest.dhall file that maps optimized filenames back to original paths.
-- The manifest is a Dhall record with optimized names as keys and original paths as values.
--
-- >>> writeManifestFile writeCap "_path_manifest.dhall" [(OptimizedName "app-config.js", OriginalPath "app/config.js")]
-- -- Creates a file with content: { `app-config.js` = "app/config.js" }
--
-- The manifest file is crucial for Claude to know where to write files back to the filesystem.
writeManifestFile :: FileWriteCap -- ^ Capability token with permissions to write to the staging directory
                  -> FilePath    -- ^ Path to the manifest file (typically "_path_manifest.dhall")
                  -> [(OptimizedName, OriginalPath)] -- ^ List of optimized name to original path mappings
                  -> ClodM ()    -- ^ Action that creates the manifest file
writeManifestFile writeCap manifestPath entries = do
  -- Create the manifest content with all entries
  let manifestLines = "{\n" : entryLines ++ ["\n}"]
      entryLines = zipWith formatEntry [0..] entries
      
      -- Format a single entry (with comma for all but the last)
      formatEntry idx (optimizedName, originalPath) =
        let comma = if idx == length entries - 1 then "" else ","
            -- Properly escape keys for Dhall format (backticks for non-standard identifiers)
            dhallOptimizedName = if needsBackticks (unOptimizedName optimizedName)
                               then "`" ++ unOptimizedName optimizedName ++ "`"
                               else unOptimizedName optimizedName
            -- Properly escape string values for Dhall
            dhallOriginalPath = "\"" ++ escapeString (unOriginalPath originalPath) ++ "\""
        in "  " ++ dhallOptimizedName ++ " = " ++ dhallOriginalPath ++ comma
  
  -- Check if path is allowed by capability
  allowed <- liftIO $ isPathAllowed (allowedWriteDirs writeCap) manifestPath
  if not allowed
    then throwError $ CapabilityError $ "Access denied: Cannot write manifest file outside allowed directories: " ++ manifestPath
    else do
      -- Write the complete manifest file at once, ensuring handles are closed promptly
      let content = unlines manifestLines
      liftIO $ System.IO.withFile manifestPath System.IO.WriteMode $ \h -> do
        System.IO.hPutStr h content
        System.IO.hFlush h
      
-- | Check if a key needs to be wrapped in backticks for Dhall
needsBackticks :: String -> Bool
needsBackticks s = case s of
  [] -> True
  (c:cs) -> not (isAlpha c) || any notAllowedInIdentifier cs
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    notAllowedInIdentifier c = not (isAlpha c || isDigit c || c == '_' || c == '-')
    isDigit c = c >= '0' && c <= '9'
    
-- | Escape string literals for Dhall
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

-- | Create an optimized filename for Claude UI
createOptimizedName :: FilePath -> OptimizedName
createOptimizedName relPath = OptimizedName finalOptimizedName
  where
    dirPart = takeDirectory relPath
    fileName = takeFileName relPath
    
    -- Handle paths with no directory part (files in root)
    finalOptimizedName = case dirPart of
      "." -> transformFilename fileName fileName
      _   -> 
        -- Process the directory part to handle hidden directories
        let dirParts = splitPath dirPart
            -- Transform each directory segment, handling hidden directories
            transformedDirParts = map transformDirPart dirParts
            -- Join them with dashes
            transformedDirPath = concat $ L.intersperse "-" transformedDirParts
            -- Transform the filename
            transformedFileName = transformFilename fileName fileName
        in transformedDirPath ++ "-" ++ transformedFileName
        
    -- Transform a directory segment, handling hidden directories
    transformDirPart :: String -> String
    transformDirPart dir = 
      -- Remove trailing slash if present
      let cleanDir = if L.isSuffixOf "/" dir then init dir else dir
      -- Apply hidden file transformation if needed
      in if not (null cleanDir) && cleanDir `startsWith` '.'
         then "dot--" ++ drop 1 cleanDir
         else cleanDir
      where
        startsWith :: String -> Char -> Bool
        startsWith [] _ = False
        startsWith (x:_) c = x == c
    -- Split a path into its directory components
    splitPath :: FilePath -> [String]
    splitPath path = filter (not . null) $ map getSegment $ L.groupBy sameGroup path
      where
        sameGroup c1 c2 = c1 /= '/' && c2 /= '/'
        getSegment seg = filter (/= '/') seg

