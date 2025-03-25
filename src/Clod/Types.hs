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
data FileResult = Success | Skipped String
  deriving (Show, Eq)

-- | Errors that can occur during operation
data ClodError = GitError String
               | FileSystemError FilePath IOError
               | ConfigError String
               | PatternError String
  deriving (Show, Eq)

-- | Monad for Clod operations
type ClodM a = ExceptT ClodError IO a

-- | Run a ClodM computation, returning either an error or a result
runClodM :: ClodM a -> IO (Either ClodError a)
runClodM = runExceptT