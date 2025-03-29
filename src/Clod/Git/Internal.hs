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
  
    -- * Testing functions
  , isGitRepo
  , getRepoRootPath
  , listModifiedFiles
  , listUntrackedFiles
  ) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, getModificationTime)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Exit ()
import Control.Exception (try, SomeException)

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
  
-- | Check if a directory is a git repository
isGitRepo :: FilePath -> IO Bool
isGitRepo path = do
  exists <- doesDirectoryExist path
  if not exists
    then return False
    else do
      let gitDir = path </> ".git"
      doesDirectoryExist gitDir

-- | Get the root path of a git repository
getRepoRootPath :: FilePath -> IO (Maybe FilePath)
getRepoRootPath path = do
  exists <- doesDirectoryExist path
  if not exists
    then return Nothing
    else do
      -- Try to run git rev-parse --show-toplevel
      result <- try $ readProcess "git" ["-C", path, "rev-parse", "--show-toplevel"] "" :: IO (Either SomeException String)
      case result of
        Left _ -> return Nothing  -- Not a git repo or git not available
        Right output -> return $ Just $ filter (/= '\n') output  -- Remove trailing newline

-- | List modified files in a git repository
listModifiedFiles :: FilePath -> IO [String]
listModifiedFiles path = do
  -- Check if it's a git repo
  isRepo <- isGitRepo path
  if not isRepo
    then return []
    else do
      -- Run git status --porcelain
      result <- try $ readProcess "git" ["-C", path, "status", "--porcelain"] "" :: IO (Either SomeException String)
      case result of
        Left _ -> return []  -- Git command failed
        Right output -> do
          -- Parse the output to find modified files
          return $ parseModifiedFiles (lines output)

-- | Parse git status output to find modified files
parseModifiedFiles :: [String] -> [String]
parseModifiedFiles = map extractFilename . filter isModified
  where
    isModified line = length line > 2 && (head line == 'M' || line !! 1 == 'M' || head line == 'A' || line !! 1 == 'A')
    extractFilename line = drop 3 line  -- Remove status and space

-- | List untracked files in a git repository
listUntrackedFiles :: FilePath -> IO [String]
listUntrackedFiles path = do
  -- Check if it's a git repo
  isRepo <- isGitRepo path
  if not isRepo
    then return []
    else do
      -- Run git ls-files --others --exclude-standard
      result <- try $ readProcess "git" ["-C", path, "ls-files", "--others", "--exclude-standard"] "" :: IO (Either SomeException String)
      case result of
        Left _ -> return []  -- Git command failed
        Right output -> return $ lines output
