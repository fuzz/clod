{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Clod.Core
-- Description : Core functionality for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides the core functionality for the Clod application,
-- including the main entry point and configuration handling.
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
--
-- === Workflow
--
-- 1. Initialize configuration (staging directory, etc.)
-- 2. Check for .clodignore or create a default one if not found
-- 3. Read ignore patterns from .gitignore and .clodignore
-- 4. Process files (all files or just modified ones)
-- 5. Generate a path manifest
-- 6. Provide next steps for using the files with Claude
--
-- This module coordinates all these steps through the 'runClod' function.

module Clod.Core
  ( -- * Main application entry point
    runClod
    
    -- * Configuration handling
  , initializeConfig
  , createStagingDir
  
    -- * File processing
  , prepareManifest
  , handleFirstRun
  ) where

import Control.Exception (try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO (hFlush, stdout)
import System.Process (callProcess, readProcess)

import Clod.Types
import Clod.Output
import Clod.FileSystem
import Clod.Git
import Clod.IgnorePatterns

-- | Default content for .clodignore file
defaultClodIgnoreContent :: String
defaultClodIgnoreContent = unlines
  [ "# Default .clodignore file for Claude uploader"
  , "# Add patterns to ignore files when uploading to Claude"
  , ""
  , "# Binary and media files"
  , "*.dll"
  , "*.dylib"
  , "*.exe"
  , "*.gif"
  , "*.ico"
  , "*.jar"
  , "*.jpg"
  , "*.jpeg"
  , "*.mp3"
  , "*.mp4"
  , "*.png"
  , "*.so"
  , "*.svg"
  , "*.tar.gz"
  , "*.zip"
  , ""
  , "# Build directories"
  , ".clod"
  , ".git"
  , "build"
  , "dist"
  , "node_modules"
  , "out"
  , "target"
  , ""
  , "# Large files and lock files"
  , "*.log"
  , "Cargo.lock"
  , "package-lock.json"
  , "pnpm-lock.yaml"
  , "yarn.lock"
  ]

-- | Main entry point for the Clod application
--
-- This function is the primary entry point for the Clod application.
-- It coordinates the entire process of preparing files for Claude AI:
-- 
-- 1. Verifies requirements (git installed, macOS or compatible platform)
-- 2. Finds the git repository root
-- 3. Initializes configuration (creating directories as needed)
-- 4. Sets up or reads .clodignore and .gitignore patterns
-- 5. Processes files based on command options
-- 6. Generates the path manifest
-- 7. Shows next steps for integrating with Claude
--
-- @
-- -- Process only modified files with default staging directory
-- runClodM $ runClod "" False True False
--
-- -- Process all files with custom staging directory
-- runClodM $ runClod "/path/to/staging" True False False
--
-- -- Run in test mode
-- runClodM $ runClod "" False False True
-- @
runClod :: FilePath  -- ^ Custom staging directory (empty for default)
        -> Bool      -- ^ Whether to process all files
        -> Bool      -- ^ Whether to process only modified files
        -> Bool      -- ^ Whether to run in test mode
        -> ClodM ()
runClod stagingDirArg allFiles modifiedFiles testModeArg = do
  -- Print version information
  liftIO $ putStrLn "clod version 1.0.0 (Haskell)"
  
  -- Check platform
  platform <- liftIO $ readProcess "uname" ["-s"] ""
  when (platform /= "Darwin\n") $ do
    printWarning $ "clod is primarily designed for macOS. Some features may not work on " ++ platform
    printWarning "Claude's filesystem access is currently only available on macOS and Windows desktop applications."
  
  -- Check for git dependency
  gitExists <- isExecutable "git"
  unless gitExists $ throwError $ GitError "git is required but not installed or not in PATH"
  
  -- Change to git repository root
  rootPath <- getRepositoryRoot
  liftIO $ setCurrentDirectory rootPath
  
  -- Check for uncommitted changes
  _ <- checkUncommittedChanges (ClodConfig rootPath "" "" "" "" "" testModeArg [])
  
  -- Initialize configuration
  config <- initializeConfig rootPath stagingDirArg testModeArg
  
  -- Check if .clodignore exists, and create it with default patterns if not
  let clodIgnorePath = rootPath </> ".clodignore"
  clodIgnoreExists <- liftIO $ doesFileExist clodIgnorePath
  unless clodIgnoreExists $ do
    liftIO $ putStrLn "Creating default .clodignore file..."
    liftIO $ writeFile clodIgnorePath defaultClodIgnoreContent
    
  -- Read .clodignore file
  clodIgnorePatterns <- readClodIgnore rootPath
  unless (null clodIgnorePatterns) $
    liftIO $ putStrLn $ "Found .clodignore with " ++ show (length clodIgnorePatterns) ++ " patterns"
    
  -- Read .gitignore file if it exists
  gitIgnorePatterns <- readGitIgnore rootPath
  unless (null gitIgnorePatterns) $
    liftIO $ putStrLn $ "Found .gitignore with " ++ show (length gitIgnorePatterns) ++ " patterns"
    
  -- Combine both sets of ignore patterns
  let ignorePatterns' = clodIgnorePatterns ++ gitIgnorePatterns
      config' = config { ignorePatterns = ignorePatterns' }
  
  liftIO $ putStrLn $ "Looking for modified files in " ++ rootPath ++ "..."
  
  -- Initialize path manifest
  let manifestPath = currentStaging config' </> "_path_manifest.json"
  liftIO $ writeFile manifestPath "{\n"
  
  -- Always create a manifest with all valid files
  liftIO $ putStrLn "Generating complete file manifest..."
  
  -- Process files based on command line arguments or interactive mode
  (fileCount, skippedCount) <- do
    lastRunExists <- liftIO $ doesFileExist (lastRunFile config')
    if lastRunExists
      then do
        liftIO $ putStrLn "Finding files modified since last run..."
        
        -- Handle options: all, modified, or interactive
        if allFiles 
          then do
            liftIO $ putStrLn "Importing all files (respecting .gitignore)..."
            processAllFiles config' manifestPath
          else if modifiedFiles || not testModeArg -- default to modified in normal mode
            then processModifiedFiles config' manifestPath
            else handleFirstRun config' manifestPath
      else do
        liftIO $ putStrLn "First run - no previous timestamp found."
        handleFirstRun config' manifestPath
  
  -- Close the path manifest JSON
  liftIO $ appendFile manifestPath "\n}"
  
  -- Update the last run marker
  liftIO $ writeFile (lastRunFile config') ""
  
  -- Handle results
  if fileCount == 0
    then do
      liftIO $ putStrLn $ "No files processed (skipped: " ++ show skippedCount ++ ")."
      -- Close the manifest file properly even if no files were processed
      -- Make sure it has proper JSON structure
      liftIO $ appendFile manifestPath "  \"_empty\": true\n}"
    else do
      -- Open the staging directory (skip in test mode)
      unless testModeArg $ do
        liftIO $ case platform of
          "Darwin\n" -> callProcess "open" [currentStaging config']
          _          -> putStrLn $ "Staging directory: " ++ currentStaging config'
      
      liftIO $ putStrLn $ "Success! " ++ show fileCount ++ " files prepared for upload. Skipped: " ++ show skippedCount
      liftIO $ putStrLn $ "Staging directory: " ++ currentStaging config'
      
      -- Show next steps
      showNextSteps config' (currentStaging config')

-- | Initialize configuration for the application
initializeConfig :: FilePath -> FilePath -> Bool -> ClodM ClodConfig
initializeConfig rootPath stagingDirArg testModeArg = do
  -- Allow user to configure staging directory
  homeDir <- liftIO getHomeDirectory
  let defaultStagingDir = homeDir </> "Claude"
  
  -- Get staging directory (from args, env var in test mode, or prompt)
  isTestMode <- if testModeArg 
                then return True
                else liftIO $ isJust <$> lookupEnv "CLOD_TEST_MODE"
                
  testStagingDir <- liftIO $ lookupEnv "CLOD_TEST_STAGING_DIR"
  
  stagingDirFinal <- 
    if not (null stagingDirArg)
      then return stagingDirArg
      else if isTestMode
        then return $ fromMaybe defaultStagingDir testStagingDir
        else do
          response <- promptUser "Staging directory" defaultStagingDir
          return response
  
  -- Config files - store in the git repo under .clod
  let configDir = rootPath </> ".clod"
      lastRunFile = configDir </> "last-run-marker"
  
  -- Create config directory if it doesn't exist
  liftIO $ createDirectoryIfMissing True configDir
  liftIO $ createDirectoryIfMissing True stagingDirFinal
  
  -- Create timestamp directory for this run
  now <- liftIO getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      currentStaging = stagingDirFinal </> ("ClaudeUpload_" ++ timestamp)
  
  liftIO $ createDirectoryIfMissing True currentStaging
  
  -- Return the complete configuration
  return $ ClodConfig
    { projectPath = rootPath
    , stagingDir = stagingDirFinal
    , configDir = configDir
    , lastRunFile = lastRunFile
    , timestamp = timestamp
    , currentStaging = currentStaging
    , testMode = isTestMode
    , ignorePatterns = []  -- Will be populated later
    }

-- | Create the staging directory structure
createStagingDir :: ClodConfig -> ClodM FilePath
createStagingDir config = do
  -- Create timestamp directory for this run
  now <- liftIO getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      currentStaging = stagingDir config </> ("ClaudeUpload_" ++ timestamp)
  
  liftIO $ createDirectoryIfMissing True currentStaging
  return currentStaging

-- | Prepare the path manifest file
prepareManifest :: FilePath -> ClodM ()
prepareManifest manifestPath = 
  liftIO $ writeFile manifestPath "{\n"

-- | Handle the first run scenario
handleFirstRun :: ClodConfig -> FilePath -> ClodM (Int, Int)
handleFirstRun config manifestPath = do
  -- In test mode, automatically choose option 'a'
  importOption <- if testMode config
                  then do
                    liftIO $ putStrLn "Test mode: automatically importing all files"
                    return 'a'
                  else do
                    liftIO $ putStrLn "Options:"
                    liftIO $ putStrLn "  a: Import all files (respecting .gitignore)"
                    liftIO $ putStrLn "  m: Import only modified files"
                    liftIO $ putStrLn "  n: Import nothing (just set timestamp)"
                    liftIO $ putStr "Choose an option [a/m/n]: "
                    liftIO $ hFlush stdout
                    opt <- liftIO getLine
                    return $ if null opt then 'n' else head opt
  
  case importOption of
    'a' -> do
      liftIO $ putStrLn "Importing all files (respecting .gitignore)..."
      processAllFiles config manifestPath
    'm' -> do
      liftIO $ putStrLn "Importing modified files..."
      processModifiedFiles config manifestPath
    _   -> do 
      liftIO $ putStrLn "Setting timestamp only."
      return (0, 0)

-- | Check if an executable exists in PATH
isExecutable :: String -> ClodM Bool
isExecutable cmd = liftIO $ isJust <$> findExecutable cmd