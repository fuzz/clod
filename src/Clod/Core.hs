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
  ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), splitDirectories)
import System.IO (writeFile)
import Data.Version (showVersion)
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import Clod.Types
import Clod.IgnorePatterns (matchesIgnorePattern)
import Clod.FileSystem.Detection (safeFileExists, safeIsTextFile)
import Clod.FileSystem.Operations (safeCopyFile)
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
  
  liftIO $ putStrLn $ "Copied: " ++ relPath ++ " → " ++ unOptimizedName finalOptimizedName
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
    
-- | Run the main Clod application
runClodApp :: ClodConfig -> FilePath -> Bool -> Bool -> Bool -> IO (Either ClodError ())
runClodApp config stagingDirArg verbose _ _ = runClodM config $ do
  when verbose $ do
    -- Print version information only in verbose mode
    liftIO $ putStrLn $ "clod version " ++ showVersion Meta.version ++ " (Haskell)"
  
  -- Execute main logic with capabilities
  mainLogic stagingDirArg verbose
    
-- | Main application logic
mainLogic :: FilePath -> Bool -> ClodM ()
mainLogic stagingDirArg verbose = do
  ClodConfig{configDir, stagingDir, projectPath, lastRunFile} <- ask
  
  -- Create directories
  liftIO $ createDirectoryIfMissing True configDir
  liftIO $ createDirectoryIfMissing True stagingDir
  
  -- Only show additional info in verbose mode
  when verbose $ do
    liftIO $ putStrLn $ "Running with capabilities, safely restricting operations to: " ++ projectPath
    liftIO $ putStrLn $ "Safe staging directory: " ++ stagingDirArg
    liftIO $ putStrLn "AI safety guardrails active with capability-based security"
  
  -- Update the last run marker to track when clod was last run
  liftIO $ System.IO.writeFile lastRunFile ""
  
  -- Output ONLY the staging directory path to stdout for piping to other tools
  -- This follows Unix principles - single line of output for easy piping
  liftIO $ putStrLn stagingDir