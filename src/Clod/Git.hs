{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Git
-- Description : Git-related operations for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides functionality for interacting with Git repositories,
-- including finding modified files and checking Git status.

module Clod.Git
  ( -- * Git repository operations
    getRepositoryRoot
  , checkUncommittedChanges
  , getGitNewFiles
  , processModifiedFiles
  , processAllFiles
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (filterM, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime, getCurrentDirectory, setCurrentDirectory, getDirectoryContents)
import System.FilePath
import System.IO (hFlush, stdout)
import System.Process (readProcess)

import Clod.Types
import Clod.FileSystem (findAllFiles, isModifiedSince, processFiles)

-- | Get the root directory of the Git repository
getRepositoryRoot :: ClodM FilePath
getRepositoryRoot = do
  result <- liftIO $ try $ readProcess "git" ["rev-parse", "--show-toplevel"] "" :: ClodM (Either SomeException String)
  case result of
    Left err -> throwError $ GitError $ "Failed to find git repository root: " ++ show err
    Right path -> return $ init path  -- Remove trailing newline

-- | Check if there are uncommitted changes in the repository
checkUncommittedChanges :: ClodConfig -> ClodM Bool
checkUncommittedChanges config = do
  result <- liftIO $ try $ readProcess "git" ["status", "--porcelain"] "" :: ClodM (Either SomeException String)
  case result of
    Left err -> throwError $ GitError $ "Failed to check git status: " ++ show err
    Right output -> do
      let hasChanges = output /= ""
      when hasChanges $ do
        liftIO $ putStrLn "Warning: You have uncommitted changes in your repository."
        liftIO $ putStrLn "It's recommended to commit your changes before running clod to ensure you can recover if needed."
        
        if testMode config
          then do
            liftIO $ putStrLn "Test mode: automatically continuing..."
            return ()
          else do
            liftIO $ putStr "Continue anyway? [y/N] "
            liftIO $ hFlush stdout
            response <- liftIO getChar
            liftIO $ putStrLn ""
            unless (response `elem` ['y', 'Y']) $ 
              throwError $ GitError "Operation cancelled by user"
            return ()
      return hasChanges

-- | Get newly added files from git that might not be caught by modification time check
getGitNewFiles :: FilePath -> ClodM [FilePath]
getGitNewFiles basePath = do
  -- Change to the directory to ensure git commands work with the correct context
  oldDir <- liftIO getCurrentDirectory
  liftIO $ setCurrentDirectory basePath
  -- Use git status to find untracked files (might not have changed modification time)
  result <- liftIO $ try $ readProcess "git" ["ls-files", "--others", "--exclude-standard"] "" :: ClodM (Either SomeException String)
  case result of
    Left err -> do
      liftIO $ putStrLn $ "Warning: Failed to get git untracked files: " ++ show err
      return []  -- If git command fails, return empty list
    Right output -> do
      -- Restore the original directory and return the results
      liftIO $ setCurrentDirectory oldDir
      -- Parse git output and return list of paths
      return $ filter (not . null) $ lines output

-- | Process only modified files since last run
-- Gets the list of files that have been modified since the last run of clod
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
      let files = filter (\f -> not (f `elem` [".", "..", ".git", ".claude-uploader"])) allFiles
      
      -- Get all files recursively from all subdirectories
      allFilesRecursive <- findAllFiles (projectPath config) files
      
      -- Filter to just the files modified since the last run
      modifiedFiles <- filterM (isModifiedSince (projectPath config) lastRunTime) allFilesRecursive
      
      -- Also use git status to find newly added files
      gitNewFiles <- getGitNewFiles (projectPath config)
      
      -- Combine modified and new files (removing duplicates)
      let allChangedFiles = L.nub $ modifiedFiles ++ gitNewFiles
      
      liftIO $ putStrLn $ "Found " ++ show (length allChangedFiles) ++ " modified files"
      
      -- Process the modified files
      processFiles config manifestPath allChangedFiles False

-- | Process all files in the directory
processAllFiles :: ClodConfig -> FilePath -> ClodM (Int, Int)
processAllFiles config manifestPath = do
  -- Get all files directly from file system
  allFiles <- liftIO $ getDirectoryContents (projectPath config)
  let files = filter (\f -> not (f `elem` [".", "..", ".git", ".claude-uploader"])) allFiles
  
  -- Get all files recursively from all subdirectories
  allFilesRecursive <- findAllFiles (projectPath config) files
  
  -- Process all files (with file copying, not just including them in manifest)
  let includeInManifestOnly = False
  processFiles config manifestPath allFilesRecursive includeInManifestOnly