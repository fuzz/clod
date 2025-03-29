{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Clod.Core
-- Description : Core functionality for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides the core functionality for the Clod application,
-- implemented using a traditional monad stack with capability-based security.
--
-- Clod (Claude Loader) is a utility for preparing and uploading files to
-- Claude AI's Project Knowledge feature. It tracks file changes, respects
-- .gitignore and .clodignore patterns, and optimizes filenames for Claude's UI.
--
-- === Main Features
--
-- * Track modified files since last run
-- * Respect .gitignore and .clodignore patterns
-- * Handle binary vs. text files
-- * Optimize filenames for Claude's UI
-- * Generate a path manifest for mapping optimized names back to original paths
-- * Capability-based security for file operations

module Clod.Core
  ( -- * Main application entry point
    runClodApp
    
    -- * File processing with capabilities
  , processFile
  , findModifiedOrAllFiles
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.FilePath ((</>), splitDirectories)
import System.IO (writeFile, stdout, stderr, hPutStrLn)
import Data.Version (showVersion)
import Control.Monad (when, filterM)

import Clod.Types
import Clod.IgnorePatterns (matchesIgnorePattern, readClodIgnore, readGitIgnore)
import Clod.FileSystem.Detection (safeFileExists, safeIsTextFile)
import Clod.FileSystem.Operations (safeCopyFile, findAllFiles)
import Clod.FileSystem.Processing (processFiles)
import qualified Clod.Git.Internal as Git
import qualified Paths_clod as Meta

-- | Check if a file should be ignored based on ignore patterns
checkIgnorePatterns :: FilePath -> FilePath -> ClodM (Either String FileResult)
checkIgnorePatterns _ relPath = do
  patterns <- ignorePatterns <$> ask
  if not (null patterns) && matchesIgnorePattern patterns relPath
    then pure $ Left "matched .clodignore pattern"
    else pure $ Right Success

-- | Check if a file exists
checkFileExists :: FileReadCap -> FilePath -> FilePath -> ClodM (Either String FileResult)
checkFileExists readCap fullPath _ = do
  exists <- safeFileExists readCap fullPath
  if exists
    then pure $ Right Success
    else pure $ Left "file does not exist"

-- | Check if a file is text
checkIsTextFile :: FileReadCap -> FilePath -> FilePath -> ClodM (Either String FileResult)
checkIsTextFile readCap fullPath _ = do
  isText <- safeIsTextFile readCap fullPath
  if isText
    then pure $ Right Success
    else pure $ Left "binary file"

-- | Copy a file to the staging directory
copyToStaging :: FileReadCap -> FileWriteCap -> FilePath -> FilePath -> ClodM (Either String FileResult)
copyToStaging readCap writeCap fullPath relPath = do
  stagingPath <- currentStaging <$> ask
  let finalOptimizedName = createOptimizedName relPath
      destPath = stagingPath </> unOptimizedName finalOptimizedName
  
  -- Copy file with optimized name using capability
  safeCopyFile readCap writeCap fullPath destPath
  
  -- Only output if verbose mode is enabled
  config <- ask
  when (verbose config) $ do
    liftIO $ hPutStrLn stderr $ "Copied: " ++ relPath ++ " → " ++ unOptimizedName finalOptimizedName
  pure $ Right Success
  where
    -- Simplified version of createOptimizedName for demonstration
    createOptimizedName :: FilePath -> OptimizedName
    createOptimizedName path = OptimizedName $ last (splitDirectories path)
    
    unOptimizedName :: OptimizedName -> String
    unOptimizedName (OptimizedName name) = name

-- | Process a file using capability-based security
processFile :: FileReadCap -> FileWriteCap -> FilePath -> FilePath -> ClodM FileResult
processFile readCap writeCap fullPath relPath = do
  let steps = [ checkIgnorePatterns fullPath relPath
              , checkFileExists readCap fullPath relPath
              , checkIsTextFile readCap fullPath relPath
              , copyToStaging readCap writeCap fullPath relPath
              ]

      -- Process steps sequentially, stopping on first error
      processSteps [] = pure $ Right Success
      processSteps (step:remaining) = do
        result <- step
        case result of
          Left reason -> pure $ Left reason
          Right _ -> processSteps remaining

  -- Run the processing pipeline and convert result
  result <- processSteps steps
  pure $ case result of
    Left reason -> Skipped reason
    Right _ -> Success
    
-- | Find modified or all files for processing
-- 
-- This function uses Git and filesystem operations to find either
-- all files or just modified files, depending on the user's choices.
findModifiedOrAllFiles :: ClodConfig -> FilePath -> Bool -> ClodM [FilePath]
findModifiedOrAllFiles _ path useAllFiles = do
  isGitRepo <- liftIO $ Git.isGitRepo path
  
  if isGitRepo
    then do
      -- If this is a git repo, use git operations
      repoRootMaybe <- liftIO $ Git.getRepoRootPath path
      case repoRootMaybe of
        Just repoRoot -> do
          if useAllFiles
            then do
              -- Get all files in the repo (using findAllFiles instead)
              -- Use empty string instead of "." to avoid "./" prefix
              findAllFiles repoRoot [""]
            else do
              -- Get modified and untracked files
              modified <- liftIO $ Git.listModifiedFiles repoRoot
              untracked <- liftIO $ Git.listUntrackedFiles repoRoot
              return $ modified ++ untracked
        Nothing -> return []
    else do
      -- If not a git repo, use filesystem operations
      if useAllFiles
        then findAllFiles path [""]  -- Use empty string instead of "." to avoid "./" prefix
        else do
          -- Get files modified since last run
          config' <- ask
          allFiles <- findAllFiles path [""]  -- Use empty string instead of "." to avoid "./" prefix
          
          -- Check if last run marker exists
          let lastRunFilePath = lastRunFile config'
          lastRunExists <- liftIO $ doesFileExist lastRunFilePath
          
          if not lastRunExists
            then return allFiles  -- If no last run marker, process all files
            else do
              -- Get the last run time from the marker file
              lastRunTime <- liftIO $ getModificationTime lastRunFilePath
              
              -- Filter files that were modified since the last run
              liftIO $ filterM (\f -> do
                let fullPath = path </> f
                fileExists <- doesFileExist fullPath
                if not fileExists
                  then return False
                  else do
                    modTime <- getModificationTime fullPath
                    return (modTime > lastRunTime)
                ) allFiles

-- | Run the main Clod application
runClodApp :: ClodConfig -> FilePath -> Bool -> Bool -> Bool -> IO (Either ClodError ())
runClodApp config _ verboseFlag optAllFiles optModified = 
  let configWithVerbose = config { verbose = verboseFlag }
  in runClodM configWithVerbose $ do
    when verboseFlag $ do
      -- Print version information only in verbose mode
      liftIO $ hPutStrLn stderr $ "clod version " ++ showVersion Meta.version ++ " (Haskell)"
    
    -- Execute main logic with capabilities
    mainLogic optAllFiles optModified
    
-- | Main application logic
mainLogic :: Bool -> Bool -> ClodM ()
mainLogic optAllFiles optModified = do
  config@ClodConfig{configDir, stagingDir, projectPath, lastRunFile, verbose} <- ask
  
  -- Create directories
  liftIO $ createDirectoryIfMissing True configDir
  liftIO $ createDirectoryIfMissing True stagingDir
  
  -- Only show additional info in verbose mode
  when verbose $ do
    liftIO $ hPutStrLn stderr $ "Running with capabilities, safely restricting operations to: " ++ projectPath
    liftIO $ hPutStrLn stderr $ "Safe staging directory: " ++ stagingDir
    liftIO $ hPutStrLn stderr "AI safety guardrails active with capability-based security"
  
  -- Load .gitignore and .clodignore patterns
  gitIgnorePatterns <- readGitIgnore projectPath
  clodIgnorePatterns <- readClodIgnore projectPath
  let allPatterns = gitIgnorePatterns ++ clodIgnorePatterns
  
  -- Create a new config with the loaded patterns
  let configWithPatterns = config { ignorePatterns = allPatterns }
  
  -- Find all eligible files in the repository
  -- Check if this is a git repository
  isGitRepo <- liftIO $ Git.isGitRepo projectPath
  
  -- Get all files to process - either all files or just modified ones
  allFiles <- if isGitRepo
              then do
                repoRootMaybe <- liftIO $ Git.getRepoRootPath projectPath
                case repoRootMaybe of
                  Just repoRoot -> findAllFiles repoRoot [""]  -- Use empty string to avoid "./" prefix
                  Nothing -> findAllFiles projectPath [""]  -- Fallback if we can't get repo root
              else findAllFiles projectPath [""]  -- Use empty string to avoid "./" prefix
  
  -- Process all files for the manifest
  let manifestPath = stagingDir </> "_path_manifest.json"
  
  -- First pass: Add all files to the manifest (manifest only mode)
  (manifestAdded, manifestSkipped) <- processFiles configWithPatterns manifestPath allFiles True
  
  when verbose $ do
    liftIO $ hPutStrLn stderr $ "Added " ++ show manifestAdded ++ " files to manifest, skipped " ++ show manifestSkipped
  
  -- Second pass: If using --modified flag, find and copy only modified files
  -- If using --all flag, also copy all files to the staging directory
  modifiedOrAllFiles <- if optModified || optAllFiles
                       then findModifiedOrAllFiles configWithPatterns projectPath optAllFiles
                       else if not (null allFiles) -- Always process files if there are any
                            then return allFiles 
                            else return []
  
  -- Process modified/all files (with actual file copying)
  (processed, skipped) <- if not (null modifiedOrAllFiles)
                        then processFiles configWithPatterns manifestPath modifiedOrAllFiles False
                        else return (0, 0)
  
  when verbose $ do
    liftIO $ hPutStrLn stderr $ "Processed " ++ show processed ++ " files, skipped " ++ show skipped ++ " files"
  
  -- Update the last run marker to track when clod was last run
  liftIO $ System.IO.writeFile lastRunFile ""
  
  -- Output ONLY the staging directory path to stdout for piping to other tools
  -- This follows Unix principles - single line of output for easy piping
  liftIO $ hPutStrLn stdout stagingDir