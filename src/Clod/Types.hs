{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : Clod.Types
-- Description : Core types for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
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
  , catchError
  , liftIO
  , ask
  , asks
  , local
  , runReaderT
  , runExceptT
  
    -- * Newtypes for type safety
  , IgnorePattern(..)
  , OptimizedName(..)
  , OriginalPath(..)
  
    -- * Capability types
  , FileReadCap(..)
  , FileWriteCap(..)
  , GitCap(..)
  , fileReadCap
  , fileWriteCap
  , gitCap
  
    -- * Path validation
  , isPathAllowed
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.List (isPrefixOf)
import System.Directory (canonicalizePath)

-- | Newtype for ignore patterns to prevent mixing with other string types
newtype IgnorePattern = IgnorePattern { unIgnorePattern :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String

-- | Newtype for optimized filename used in Claude's UI
newtype OptimizedName = OptimizedName { unOptimizedName :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String

-- | Newtype for original filepath in the repository
newtype OriginalPath = OriginalPath { unOriginalPath :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String

-- | Configuration for the clod program
data ClodConfig = ClodConfig
  { projectPath    :: !FilePath      -- ^ Root path of the git repository
  , stagingDir     :: !FilePath      -- ^ Directory where files will be staged for Claude
  , configDir      :: !FilePath      -- ^ Directory for configuration files
  , lastRunFile    :: !FilePath      -- ^ File that marks when clod was last run
  , timestamp      :: !String        -- ^ Timestamp for the current run
  , currentStaging :: !FilePath      -- ^ Path to the current staging directory
  , testMode       :: !Bool          -- ^ Whether we're running in test mode
  , verbose        :: !Bool          -- ^ Whether to print verbose output
  , ignorePatterns :: ![IgnorePattern] -- ^ Patterns from .gitignore and .clodignore
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Typeable)

-- | Result of processing a file
-- 
-- * 'Success' indicates the file was successfully processed and included
-- * 'Skipped' indicates the file was skipped with a reason (matched ignore pattern, binary file, etc.)
data FileResult 
  = Success              -- ^ File was successfully processed
  | Skipped !String      -- ^ File was skipped with the given reason
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Typeable)

-- | Errors that can occur during Clod operation
--
-- These represent the different categories of errors that can occur during
-- file processing, allowing for specific error handling for each case.
data ClodError 
  = GitError !String                   -- ^ Error related to Git operations (e.g., cannot find repo)
  | FileSystemError !FilePath !IOError -- ^ Error related to filesystem operations
  | ConfigError !String                -- ^ Error related to configuration (e.g., invalid settings)
  | PatternError !String               -- ^ Error related to pattern matching (e.g., invalid pattern)
  | CapabilityError !String           -- ^ Error related to capability validation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Typeable)

-- | The Clod monad
--
-- This monad stack combines:
--
-- * Reader for dependency injection of ClodConfig
-- * Error handling with ExceptT for 'ClodError'
-- * IO for filesystem, git, and other side effects
--
-- This replaces the previous effects-based approach with a simpler,
-- more traditional monad stack.
type ClodM a = ReaderT ClodConfig (ExceptT ClodError IO) a

-- | Run a ClodM computation, returning either an error or a result
runClodM :: ClodConfig -> ClodM a -> IO (Either ClodError a)
runClodM config action = runExceptT (runReaderT action config)

-- | Capability for reading files within certain directories
data FileReadCap = FileReadCap 
  { allowedReadDirs :: [FilePath] -- ^ Directories where reading is permitted
  } deriving (Show, Eq)

-- | Capability for writing files within certain directories
data FileWriteCap = FileWriteCap 
  { allowedWriteDirs :: [FilePath] -- ^ Directories where writing is permitted
  } deriving (Show, Eq)

-- | Capability for Git operations
data GitCap = GitCap 
  { allowedRepos :: [FilePath] -- ^ Git repositories where operations are permitted
  } deriving (Show, Eq)

-- | Create a file read capability for specified directories
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap dirs = FileReadCap { allowedReadDirs = dirs }

-- | Create a file write capability for specified directories
fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap dirs = FileWriteCap { allowedWriteDirs = dirs }

-- | Create a Git capability for specified repositories
gitCap :: [FilePath] -> GitCap
gitCap dirs = GitCap { allowedRepos = dirs }

-- | Check if a path is within allowed directories
-- This improved version handles path traversal attacks by comparing canonical paths
isPathAllowed :: [FilePath] -> FilePath -> IO Bool
isPathAllowed allowedDirs path = do
  -- Get canonical paths to resolve any `.`, `..`, or symlinks
  canonicalPath <- canonicalizePath path
  -- Check if the canonical path is within any of the allowed directories
  checks <- mapM (\dir -> do
                   canonicalDir <- canonicalizePath dir
                   -- A path is allowed if:
                   -- 1. It equals an allowed directory exactly, or
                   -- 2. It's a proper subdirectory (dir is a prefix and has a path separator)
                   let isAllowed = canonicalDir == canonicalPath || 
                                  (canonicalDir `isPrefixOf` canonicalPath && 
                                   length canonicalPath > length canonicalDir &&
                                   isPathSeparator (canonicalPath !! length canonicalDir))
                   return isAllowed) allowedDirs
  -- Return result
  return (or checks)
  where
    isPathSeparator c = c == '/' || c == '\\'