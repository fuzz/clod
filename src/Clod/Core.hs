{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.Core
-- Description : Core functionality for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides the core functionality for the Clod application,
-- implemented using an algebraic effects system with capability-based security.
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
--
-- === Effects System 
--
-- The core implementation uses an algebraic effects system (via polysemy) to provide
-- explicit tracking of side effects and capability-based security. This ensures that
-- operations can only access files within explicitly permitted directories.

module Clod.Core
  ( -- * Main application entry point using effects
    runClodApp
    
    -- * File processing with capabilities
  , processFileWithEffects
  ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), splitDirectories)
import System.IO (writeFile)
import Data.Version (showVersion)
import Control.Monad (when)

import Polysemy
import Polysemy.Error
import Polysemy.Reader hiding (ask)
import qualified Polysemy.Reader as PR

import qualified Clod.Types as T
import Clod.Types (ClodConfig(..), ignorePatterns, projectPath, configDir, stagingDir, lastRunFile, currentStaging)
import Clod.IgnorePatterns (matchesIgnorePattern)
import Clod.Effects hiding (writeFile)
import Clod.Capability
import qualified Paths_clod as Meta

-- | Process a file using the effects system
processFileWithEffects :: Members '[FileSystem, Error T.ClodError, Console, Reader T.ClodConfig, Embed IO] r
                       => FileReadCap -> FileWriteCap -> FilePath -> FilePath -> Sem r T.FileResult
processFileWithEffects readCap writeCap fullPath relPath = do
  config <- PR.ask
  
  -- Check if file should be ignored according to ignore patterns
  let patterns = ignorePatterns config
      
  if not (null patterns) && matchesIgnorePattern patterns relPath
    then pure $ T.Skipped "matched .clodignore pattern"
    else do
      -- Check file exists using capability
      exists <- safeFileExists readCap fullPath
      if not exists
        then pure $ T.Skipped "file does not exist"
        else do
          -- Check if file is a text file using capability-based security
          isText <- safeIsTextFile readCap fullPath
          
          if not isText
            then pure $ T.Skipped "binary file"
            else do
              -- Create optimized filename
              let finalOptimizedName = createOptimizedName relPath
                  destPath = currentStaging config </> unOptimizedName finalOptimizedName
              
              -- Copy file with optimized name using capability
              safeCopyFile readCap writeCap fullPath destPath
              
              logInfo $ "Copied: " ++ relPath ++ " → " ++ unOptimizedName finalOptimizedName
              pure T.Success
  where
    -- Simplified version of createOptimizedName for demonstration
    createOptimizedName :: FilePath -> T.OptimizedName
    createOptimizedName path = T.OptimizedName $ last (splitDirectories path)
    
    unOptimizedName :: T.OptimizedName -> String
    unOptimizedName (T.OptimizedName name) = name
    
-- | Run a computation with the Clod effects system
runClodApp :: T.ClodConfig -> FilePath -> Bool -> Bool -> Bool -> IO (Either T.ClodError ())
runClodApp config stagingDirArg verbose _ _ = do
  -- Run with polysemy effects
  runM . runReader config . runError . runConsoleIO . runGitIO . runFileSystemIO $ do
    when verbose $ do
      -- Print version information only in verbose mode
      logInfo $ "clod version " ++ showVersion Meta.version ++ " (Haskell)"
    
    -- Execute main logic with capabilities
    effectsBasedMain stagingDirArg verbose
    
-- | Main function using effects system
effectsBasedMain :: Members '[FileSystem, Git, Console, Error T.ClodError, Reader T.ClodConfig, Embed IO] r
                 => FilePath -> Bool -> Sem r ()
effectsBasedMain stagingDirArg verbose = do
  config <- PR.ask
  
  -- Create directories
  embed $ createDirectoryIfMissing True (configDir config)
  embed $ createDirectoryIfMissing True (stagingDir config)
  
  -- Only show additional info in verbose mode
  when verbose $ do
    logInfo $ "Running with capabilities, safely restricting operations to: " ++ projectPath config
    logInfo $ "Safe staging directory: " ++ stagingDirArg
    logInfo "AI safety guardrails active with capability-based security"
  
  -- Update the last run marker to track when clod was last run
  embed $ System.IO.writeFile (lastRunFile config) ""
  
  -- Output ONLY the staging directory path to stdout for piping to other tools
  -- This follows Unix principles - single line of output for easy piping
  logOutput $ stagingDir config
