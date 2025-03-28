{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Clod.Core
-- Description : Core functionality for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
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
  , createTempStagingDir
  , cleanupPreviousTempDir
  
    -- * File processing
  , prepareManifest
  , handleFirstRun
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless, when)
import Data.Maybe (isJust)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.IO (hFlush, stdout)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (callProcess)
import Data.List (isSuffixOf)
import Data.Version (showVersion)

import Clod.Types
import Clod.Output
import Clod.Git
import Clod.IgnorePatterns
import Clod.Config (clodIgnoreFile, clodConfigDir)
import qualified Paths_clod as Meta

-- | Path to store the last temporary directory location
lastTempDirFile :: ClodConfig -> FilePath
lastTempDirFile config = configDir config </> "last-temp-dir"

-- | Load default content for .clodignore file from resources
loadDefaultClodIgnoreContent :: IO String
loadDefaultClodIgnoreContent = do
  -- Try to find the resource file in various locations
  execPath <- getExecutablePath
  let execDir = takeDirectory execPath
      
      -- Potential locations for the resource file
      possiblePaths = [
          "resources/default_clodignore",                     -- Current working directory
          execDir </> "resources/default_clodignore",         -- Executable directory
          execDir </> "../resources/default_clodignore",      -- One level up from executable
          execDir </> "../share/clod/default_clodignore",     -- Typical Linux package install location
          "/usr/local/share/clod/default_clodignore",         -- Standard location for system-wide resources
          "/usr/share/clod/default_clodignore"                -- Alternative location
        ]
  
  -- Try each path until we find one that exists
  findExistingFile possiblePaths
  
  where
    findExistingFile :: [FilePath] -> IO String
    findExistingFile [] = return defaultFallbackContent  -- Fallback if no file is found
    findExistingFile (p:ps) = do
      exists <- doesFileExist p
      if exists
        then readFile p
        else findExistingFile ps
        
    -- Minimal fallback content in case the resource file is not found
    defaultFallbackContent = unlines [
        "# Default .clodignore file for Claude uploader",
        "# Add patterns to ignore files when uploading to Claude",
        "",
        "# Build directories",
        ".clod",
        ".git",
        "build",
        "dist",
        "node_modules",
        "",
        "# Binary and media files",
        "*.exe",
        "*.dll",
        "*.so",
        "*.dylib",
        "*.zip",
        "*.tar.gz",
        "*.jpg",
        "*.png"
      ]

-- | Main entry point for the Clod application
--
-- This function is the primary entry point for the Clod application.
-- It coordinates the entire process of preparing files for Claude AI:
-- 
-- 1. Verifies requirements (git installed, macOS or compatible platform)
-- 2. Finds the git repository root
-- 3. Initializes configuration with temporary directory support
-- 4. Sets up or reads .clodignore and .gitignore patterns
-- 5. Processes files based on command options
-- 6. Generates the path manifest
-- 7. Shows next steps for integrating with Claude
--
-- @
-- -- Process only modified files
-- runClodM $ runClod "" False True False
--
-- -- Process all files
-- runClodM $ runClod "" True False False
--
-- -- Run in test mode (with possible test directory override)
-- runClodM $ runClod "" False False True
-- @
runClod :: FilePath  -- ^ Test mode staging directory (only used in test mode)
        -> Bool      -- ^ Whether to process all files
        -> Bool      -- ^ Whether to process only modified files
        -> Bool      -- ^ Whether to run in test mode
        -> ClodM ()
runClod stagingDirArg allFiles modifiedFiles testModeArg = do
  -- Print version information from cabal file using auto-generated Paths_clod module
  liftIO $ putStrLn $ "clod version " ++ showVersion Meta.version ++ " (Haskell)"
  
  -- Check dependencies
  ensureGitInstalled
  
  -- Change to git repository root
  rootPath <- getRepositoryRoot
  liftIO $ setCurrentDirectory rootPath
  
  -- Create a temporary config just for checking uncommitted changes
  let tempConfig = ClodConfig rootPath "" "" "" "" "" testModeArg []
  _ <- checkUncommittedChanges tempConfig
  
  -- Initialize configuration with temporary directory support
  config <- initializeTempConfig rootPath stagingDirArg testModeArg
  
  -- Setup and load ignore patterns
  config' <- setupIgnorePatterns rootPath config
  
  liftIO $ putStrLn $ "Looking for modified files in " ++ rootPath ++ "..."
  
  -- Initialize and process files
  let manifestPath = currentStaging config' </> "_path_manifest.json"
  liftIO $ writeFile manifestPath "{\n"
  liftIO $ putStrLn "Generating complete file manifest..."
  
  -- Process files and finalize  
  (fileCount, skippedCount) <- processAppropriateFiles config' manifestPath allFiles modifiedFiles
  finalizeManifest manifestPath config' fileCount skippedCount ""
  
  where
      
    -- Ensure git is installed
    ensureGitInstalled :: ClodM ()
    ensureGitInstalled = do
      gitExists <- isExecutable "git"
      unless gitExists $ throwError $ GitError "git is required but not installed or not in PATH"
      
    -- Setup and load ignore patterns
    setupIgnorePatterns :: FilePath -> ClodConfig -> ClodM ClodConfig
    setupIgnorePatterns rootPath config = do
      ignoreFileName <- liftIO clodIgnoreFile
      let clodIgnorePath = rootPath </> ignoreFileName
      
      -- Create .clodignore if it doesn't exist
      clodIgnoreExists <- liftIO $ doesFileExist clodIgnorePath
      unless clodIgnoreExists $ do
        liftIO $ putStrLn $ "Creating default " ++ ignoreFileName ++ " file..."
        defaultContent <- liftIO loadDefaultClodIgnoreContent
        liftIO $ writeFile clodIgnorePath defaultContent
      
      -- Read patterns
      clodPatterns <- readClodIgnore rootPath
      unless (null clodPatterns) $
        liftIO $ putStrLn $ "Found .clodignore with " ++ show (length clodPatterns) ++ " patterns"
      
      -- Read root .gitignore
      rootGitPatterns <- readGitIgnore rootPath
      unless (null rootGitPatterns) $
        liftIO $ putStrLn $ "Found .gitignore with " ++ show (length rootGitPatterns) ++ " patterns"
      
      -- Read nested .gitignore files from subdirectories
      nestedGitPatterns <- readNestedGitIgnores rootPath
      let (inclusion, negation) = categorizePatterns nestedGitPatterns
      unless (null nestedGitPatterns) $
        liftIO $ putStrLn $ "Found " ++ show (length nestedGitPatterns) ++ " patterns in nested .gitignore files"
        ++ " (" ++ show (length inclusion) ++ " inclusions, " ++ show (length negation) ++ " negations)"
      
      -- Combine patterns (order matters - later patterns take precedence)
      let allPatterns = clodPatterns ++ rootGitPatterns ++ nestedGitPatterns
      
      -- Return updated config
      return $ config { ignorePatterns = allPatterns }
      
    -- Process appropriate files based on options and state
    processAppropriateFiles :: ClodConfig -> FilePath -> Bool -> Bool -> ClodM (Int, Int)
    processAppropriateFiles config manifestPath allFilesFlag modifiedFilesFlag = do
      lastRunExists <- liftIO $ doesFileExist (lastRunFile config)
      
      if lastRunExists
        then do
          liftIO $ putStrLn "Finding files modified since last run..."
          case (allFilesFlag, modifiedFilesFlag || not (testMode config)) of
            (True, _) -> do
              liftIO $ putStrLn "Importing all files (respecting .gitignore)..."
              processAllFiles config manifestPath
            (_, True) -> 
              processModifiedFiles config manifestPath
            _ -> 
              handleFirstRun config manifestPath
        else do
          liftIO $ putStrLn "First run - no previous timestamp found."
          handleFirstRun config manifestPath
          
    -- Finalize manifest and show results
    finalizeManifest :: FilePath -> ClodConfig -> Int -> Int -> String -> ClodM ()
    finalizeManifest manifestPath config fileCount skippedCount _ = do
      -- Close the path manifest JSON
      liftIO $ appendFile manifestPath "\n}"
      
      -- Update the last run marker
      liftIO $ writeFile (lastRunFile config) ""
      
      -- Handle results based on file count
      if fileCount == 0
        then do
          liftIO $ putStrLn $ "No files processed (skipped: " ++ show skippedCount ++ ")."
          -- Ensure proper JSON structure even with no files
          liftIO $ appendFile manifestPath "  \"_empty\": true\n}"
        else do
          -- Open the staging directory (skip in test mode)
          unless (testMode config) $ do
            -- This uses 'open' command which is available on macOS
            -- Other platforms may need to configure alternatives
            liftIO $ callProcess "open" [currentStaging config]
          
          liftIO $ putStrLn $ "Success! " ++ show fileCount ++ 
                              " files prepared for upload. Skipped: " ++ show skippedCount
          liftIO $ putStrLn $ "Staging directory: " ++ currentStaging config
          
          -- Show next steps
          showNextSteps config (currentStaging config)

-- | Clean up the previous temporary directory if it exists
cleanupPreviousTempDir :: ClodConfig -> ClodM ()
cleanupPreviousTempDir config = do
  -- Check if we have a record of a previous temp directory
  prevDirExists <- liftIO $ doesFileExist (lastTempDirFile config)
  
  when prevDirExists $ do
    -- Read the previous directory path
    prevDirPath <- liftIO $ readFile (lastTempDirFile config)
    
    -- Check if that directory still exists
    dirExists <- liftIO $ doesDirectoryExist prevDirPath
    
    when dirExists $ do
      -- Try to remove the directory and its contents
      result <- liftIO $ try $ removeDirectoryRecursive prevDirPath
      
      case result of
        Right _ -> 
          liftIO $ putStrLn $ "Cleaned up previous staging directory: " ++ prevDirPath
        Left err -> 
          liftIO $ putStrLn $ "Warning: Could not clean up previous staging directory: " ++ 
                               show (err :: SomeException)

-- | Create a temporary staging directory for this run
-- Uses system temporary directory that will be cleaned up on reboot
createTempStagingDir :: ClodConfig -> ClodM FilePath
createTempStagingDir config = do
  -- Get the system's canonical temporary directory
  tmpBaseDir <- liftIO getCanonicalTemporaryDirectory
  
  -- Create a timestamped directory name
  let dirPrefix = "clod_" ++ timestamp config ++ "_"
  
  -- Create the temporary directory
  stagingPath <- liftIO $ createTempDirectory tmpBaseDir dirPrefix
  
  -- Store this path for cleanup on next run
  liftIO $ writeFile (lastTempDirFile config) stagingPath
  
  -- Return the created directory path
  return stagingPath

-- | Initialize configuration for the application with temporary directory support
--
-- This function initializes the configuration for Clod by:
-- 
-- 1. Creating a timestamped configuration
-- 2. Setting up the .clod directory for configuration files
-- 3. Cleaning up any previous temporary directories
-- 4. Creating a new temporary staging directory for the current run
--
-- In test mode, it can use a specified directory instead of a temporary one.
initializeTempConfig :: FilePath -> FilePath -> Bool -> ClodM ClodConfig
initializeTempConfig rootPath stagingDirArg testModeArg = do
  -- Initialize configuration with temporary directory support
  
  -- Get current time for timestamp
  now <- liftIO getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
  
  -- Config files - store in the git repo under configured directory
  configDir <- liftIO $ clodConfigDir rootPath
  let lastRunFile = configDir </> "last-run-marker"
  
  -- Create config directory if it doesn't exist
  liftIO $ createDirectoryIfMissing True configDir
  
  -- Create a base config with empty staging dir (will be filled in later)
  let baseConfig = ClodConfig
        { projectPath = rootPath
        , stagingDir = ""  -- Will be set later
        , configDir = configDir
        , lastRunFile = lastRunFile
        , timestamp = timestamp
        , currentStaging = ""  -- Will be set later
        , testMode = testModeArg
        , ignorePatterns = []  -- Will be populated later
        }
  
  -- Clean up previous temp directory if possible
  cleanupPreviousTempDir baseConfig
  
  -- Get staging directory based on mode
  stagingDir <- if testModeArg && not (null stagingDirArg)
                then do
                  -- For test mode, use the provided directory if specified
                  liftIO $ createDirectoryIfMissing True stagingDirArg
                  return stagingDirArg
                else do
                  -- Create a temporary directory
                  createTempStagingDir baseConfig
  
  -- Return the complete configuration
  return $ baseConfig
    { stagingDir = stagingDir
    , currentStaging = stagingDir  -- For temp dirs, these are the same
    }

-- | Initialize configuration for the application
-- 
-- This function is kept for backward compatibility with tests and other modules
-- but internally calls initializeTempConfig for the actual implementation
initializeConfig :: FilePath -> FilePath -> Bool -> ClodM ClodConfig
initializeConfig rootPath stagingDirArg testModeArg =
  initializeTempConfig rootPath stagingDirArg testModeArg

-- | Return the staging directory path
-- 
-- This function is kept for backward compatibility with tests and other modules.
-- Unlike the previous implementation that would create a directory structure,
-- this now simply returns the path to the temporary staging directory that was
-- already created during initialization.
createStagingDir :: ClodConfig -> ClodM FilePath
createStagingDir config = return (stagingDir config)

-- | Prepare the path manifest file
prepareManifest :: FilePath -> ClodM ()
prepareManifest manifestPath = 
  liftIO $ writeFile manifestPath "{\n"

-- | Handle the first run scenario
handleFirstRun :: ClodConfig -> FilePath -> ClodM (Int, Int)
handleFirstRun config manifestPath = do
  -- Determine import option based on mode
  importOption <- getImportOption (testMode config)
  
  -- Process files based on option
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
  where
    -- Helper function to get import option based on mode
    getImportOption :: Bool -> ClodM Char
    getImportOption True = do
      liftIO $ putStrLn "Test mode: automatically importing all files"
      return 'a'
    getImportOption False = do
      liftIO $ putStrLn "Options:"
      liftIO $ putStrLn "  a: Import all files (respecting .gitignore)"
      liftIO $ putStrLn "  m: Import only modified files"
      liftIO $ putStrLn "  n: Import nothing (just set timestamp)"
      liftIO $ putStr "Choose an option [a/m/n]: "
      liftIO $ hFlush stdout
      userInput <- liftIO getLine
      return $ case userInput of
        []    -> 'n'  -- Default to 'n' for empty input
        (c:_) -> c    -- Take first character of input

-- | Check if an executable exists in PATH
isExecutable :: String -> ClodM Bool
isExecutable cmd = liftIO $ isJust <$> findExecutable cmd