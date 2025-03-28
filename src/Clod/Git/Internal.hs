{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Git.Internal
-- Description : Internal Git operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides internal Git operations that should not be directly
-- exposed to users of the library. It contains helper functions and utilities
-- used by other Git modules.

module Clod.Git.Internal
  ( -- * File processing
    processModifiedFiles
  , processAllFiles
  ) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import System.Directory (doesFileExist, getDirectoryContents, getModificationTime)

import Clod.Config (configDirName)
import Clod.FileSystem (findAllFiles, isModifiedSince, processFiles)
import Clod.Git.LibGit (getModifiedFiles, getUntrackedFiles)
import Clod.Types (ClodM, ClodConfig(..))

-- | Process only modified files since last run
--
-- This function identifies and processes files that have been modified
-- since the last run of Clod. It combines two detection methods:
--
-- 1. Check file modification times against the last run timestamp
-- 2. Get untracked files from Git that wouldn't be caught by timestamp checking
--
-- This approach ensures that all relevant changes are detected.
-- If no last run marker exists, it falls back to processing all files.
--
-- @
-- (processedCount, skippedCount) <- processModifiedFiles config manifestPath
-- @
processModifiedFiles :: ClodConfig -> FilePath -> ClodM (Int, Int)
processModifiedFiles config manifestPath = do
  -- Check if the lastRunFile exists
  lastRunExists <- liftIO $ doesFileExist (lastRunFile config)
  
  if not lastRunExists
    then do
      liftIO $ putStrLn "No last run marker found - considering all files as modified"
      processAllFiles config manifestPath
    else do
      -- Get last run file's modification time
      lastRunTime <- liftIO $ getModificationTime (lastRunFile config)
      liftIO $ putStrLn $ "Last run time: " ++ show lastRunTime
      
      -- Get all files from the repository
      allFiles <- liftIO $ getDirectoryContents (projectPath config)
      configDirName' <- liftIO configDirName
      let files = filter (\f -> not (f `elem` [".", "..", ".git", configDirName'])) allFiles
      
      -- Get all files recursively from all subdirectories
      allFilesRecursive <- findAllFiles (projectPath config) files
      
      -- Filter to just the files modified since the last run
      modifiedFiles <- filterM (isModifiedSince (projectPath config) lastRunTime) allFilesRecursive
      
      -- Also use git status to find newly added files
      gitNewFiles <- getUntrackedFiles (projectPath config)
      gitModifiedFiles <- getModifiedFiles (projectPath config)
      
      -- Combine modified and new files (removing duplicates)
      let allChangedFiles = L.nub $ modifiedFiles ++ gitNewFiles ++ gitModifiedFiles
      
      liftIO $ putStrLn $ "Found " ++ show (length allChangedFiles) ++ " modified files"
      
      -- Process the modified files
      processFiles config manifestPath allChangedFiles False

-- | Process all files in the directory
processAllFiles :: ClodConfig -> FilePath -> ClodM (Int, Int)
processAllFiles config manifestPath = do
  -- Get all files directly from file system
  allFiles <- liftIO $ getDirectoryContents (projectPath config)
  configDirName' <- liftIO configDirName
  let files = filter (\f -> not (f `elem` [".", "..", ".git", configDirName'])) allFiles
  
  -- Get all files recursively from all subdirectories
  allFilesRecursive <- findAllFiles (projectPath config) files
  
  -- Process all files (with file copying, not just including them in manifest)
  let includeInManifestOnly = False
  processFiles config manifestPath allFilesRecursive includeInManifestOnly