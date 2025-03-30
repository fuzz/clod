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
{-# LANGUAGE InstanceSigs #-}

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
  , FileEntry(..)
  , ClodDatabase(..)
  , SerializableClodDatabase(..)
  , toSerializable
  , fromSerializable
  
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
  , Checksum(..)
  
    -- * Capability types
  , FileReadCap(..)
  , FileWriteCap(..)
  , fileReadCap
  , fileWriteCap
  
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
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Dhall (FromDhall, ToDhall)

-- | Newtype for ignore patterns to prevent mixing with other string types
newtype IgnorePattern = IgnorePattern { unIgnorePattern :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String

-- | Newtype for optimized filename used in Claude's UI
newtype OptimizedName = OptimizedName { unOptimizedName :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String
  deriving (Generic)

instance FromDhall OptimizedName
instance ToDhall OptimizedName

-- | Newtype for original filepath in the repository
newtype OriginalPath = OriginalPath { unOriginalPath :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String
  deriving (Generic)

instance FromDhall OriginalPath
instance ToDhall OriginalPath

-- | Configuration for the clod program
data ClodConfig = ClodConfig
  { projectPath    :: !FilePath      -- ^ Root path of the project
  , stagingDir     :: !FilePath      -- ^ Directory where files will be staged for Claude
  , configDir      :: !FilePath      -- ^ Directory for configuration files
  , databaseFile   :: !FilePath      -- ^ Path to the checksums database file
  , timestamp      :: !String        -- ^ Timestamp for the current run
  , currentStaging :: !FilePath      -- ^ Path to the current staging directory
  , previousStaging :: !(Maybe FilePath) -- ^ Path to the previous staging directory, if any
  , testMode       :: !Bool          -- ^ Whether we're running in test mode
  , verbose        :: !Bool          -- ^ Whether to print verbose output
  , flushMode      :: !Bool          -- ^ Whether to flush stale entries from the database
  , lastMode       :: !Bool          -- ^ Whether to use the previous staging directory
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
  = FileSystemError !FilePath !IOError -- ^ Error related to filesystem operations
  | ConfigError !String                -- ^ Error related to configuration (e.g., invalid settings)
  | PatternError !String               -- ^ Error related to pattern matching (e.g., invalid pattern)
  | CapabilityError !String            -- ^ Error related to capability validation
  | DatabaseError !String              -- ^ Error related to checksums database
  | ChecksumError !String              -- ^ Error related to checksum calculation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Typeable)

-- | Newtype for file checksums to prevent mixing with other string types
newtype Checksum = Checksum { unChecksum :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String
  deriving (Generic)

instance FromDhall Checksum
instance ToDhall Checksum

-- | File entry in the checksum database
data FileEntry = FileEntry
  { entryPath         :: !FilePath       -- ^ Original path
  , entryChecksum     :: !Checksum       -- ^ File content checksum
  , entryLastModified :: !UTCTime        -- ^ Last modified time
  , entryOptimizedName :: !OptimizedName -- ^ Name in staging directory
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Typeable, FromDhall, ToDhall)

-- | Main database structure
data ClodDatabase = ClodDatabase
  { dbFiles          :: !(Map FilePath FileEntry)  -- ^ All tracked files by path
  , dbChecksums      :: !(Map String FilePath)     -- ^ Mapping from checksum to path (for rename detection)
  , dbLastStagingDir :: !(Maybe FilePath)          -- ^ Previous staging directory
  , dbLastRunTime    :: !UTCTime                  -- ^ Time of last run
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Typeable)

-- | Serialization-friendly version of ClodDatabase
data SerializableClodDatabase = SerializableClodDatabase
  { serializedFiles          :: ![(FilePath, FileEntry)]  -- ^ All tracked files as pairs
  , serializedChecksums      :: ![(String, FilePath)]     -- ^ Checksums as pairs
  , serializedLastStagingDir :: !(Maybe FilePath)          -- ^ Previous staging directory
  , serializedLastRunTime    :: !UTCTime                  -- ^ Time of last run
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (Typeable, FromDhall, ToDhall)

-- | Convert to serializable form
toSerializable :: ClodDatabase -> SerializableClodDatabase
toSerializable db = SerializableClodDatabase
  { serializedFiles = Map.toList (dbFiles db)
  , serializedChecksums = Map.toList (dbChecksums db)
  , serializedLastStagingDir = dbLastStagingDir db
  , serializedLastRunTime = dbLastRunTime db
  }

-- | Convert from serializable form
fromSerializable :: SerializableClodDatabase -> ClodDatabase
fromSerializable sdb = ClodDatabase
  { dbFiles = Map.fromList (serializedFiles sdb)
  , dbChecksums = Map.fromList (serializedChecksums sdb)
  , dbLastStagingDir = serializedLastStagingDir sdb
  , dbLastRunTime = serializedLastRunTime sdb
  }

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

-- | Create a file read capability for specified directories
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap dirs = FileReadCap { allowedReadDirs = dirs }

-- | Create a file write capability for specified directories
fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap dirs = FileWriteCap { allowedWriteDirs = dirs }

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