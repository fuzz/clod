{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Clod.Types
-- Description : Core types for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module defines the core types used throughout the Clod application.
-- Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge
-- feature. It tracks file changes, respects .gitignore and .clodignore patterns, and
-- optimizes filenames for Claude's UI.
--
-- The primary types include:
--
-- * 'ClodConfig' - Configuration for file processing and staging
-- * 'ClodM' - A monad for handling errors during file operations
-- * 'ClodError' - Various error types that can occur during operation
-- * 'FileResult' - Result of processing a file (success or skipped)

module Clod.Types
  ( -- * Core Types
    ClodConfig(..)
  , FileResult(..)
  , ClodError(..)
  , ClodM
  
    -- * Type conversions and runners
  , runClodM
  , throwError
  , liftIO
  
    -- * Type Aliases
  , IgnorePattern
  , OptimizedName
  , OriginalPath
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (UTCTime)
import System.IO.Error (IOError)

-- | Type alias for ignore patterns
type IgnorePattern = String

-- | Type alias for optimized filename used in Claude's UI
type OptimizedName = String

-- | Type alias for original filepath in the repository
type OriginalPath = String

-- | Configuration for the clod program
data ClodConfig = ClodConfig
  { projectPath   :: FilePath      -- ^ Root path of the git repository
  , stagingDir    :: FilePath      -- ^ Directory where files will be staged for Claude
  , configDir     :: FilePath      -- ^ Directory for configuration files
  , lastRunFile   :: FilePath      -- ^ File that marks when clod was last run
  , timestamp     :: String        -- ^ Timestamp for the current run
  , currentStaging :: FilePath     -- ^ Path to the current staging directory
  , testMode      :: Bool          -- ^ Whether we're running in test mode
  , ignorePatterns :: [IgnorePattern] -- ^ Patterns from .gitignore and .clodignore
  }

-- | Result of processing a file
-- 
-- * 'Success' indicates the file was successfully processed and included
-- * 'Skipped' indicates the file was skipped with a reason (matched ignore pattern, binary file, etc.)
data FileResult = Success    -- ^ File was successfully processed
                | Skipped String  -- ^ File was skipped with the given reason
  deriving (Show, Eq)

-- | Errors that can occur during Clod operation
--
-- These represent the different categories of errors that can occur during
-- file processing, allowing for specific error handling for each case.
data ClodError = GitError String  -- ^ Error related to Git operations (e.g., cannot find repo)
               | FileSystemError FilePath IOError  -- ^ Error related to filesystem operations
               | ConfigError String  -- ^ Error related to configuration (e.g., invalid settings)
               | PatternError String  -- ^ Error related to pattern matching (e.g., invalid pattern)
  deriving (Show, Eq)

-- | Monad for Clod operations
--
-- This is a monad transformer stack that combines:
--
-- * Error handling with ExceptT for 'ClodError'
-- * IO for filesystem, git, and other side effects
--
-- All Clod operations that can fail or require IO should use this monad.
-- It allows for clean error propagation and handling.
type ClodM a = ExceptT ClodError IO a

-- | Run a ClodM computation, returning either an error or a result
--
-- Example:
--
-- @
-- result <- runClodM $ do
--   config <- initializeConfig "/path/to/repo" "" False
--   files <- findAllFiles (projectPath config) ["src"]
--   processFiles config manifestPath files False
-- @
runClodM :: ClodM a -> IO (Either ClodError a)
runClodM = runExceptT