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
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Clod.Types
-- Description : Core types for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
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
    
    -- * Lens operators and accessors
  , (^.), (.~), (%~), (&)
    
    -- * Field lenses for ClodConfig
  , projectPath
  , stagingDir
  , configDir
  , databaseFile
  , timestamp
  , currentStaging
  , previousStaging
  , testMode
  , verbose
  , flushMode
  , lastMode
  , ignorePatterns
    
    -- * Field lenses for ClodDatabase
  , dbFiles
  , dbChecksums
  , dbLastStagingDir
  , dbLastRunTime
    
    -- * Field lenses for FileEntry
  , entryPath
  , entryChecksum
  , entryLastModified
  , entryOptimizedName
    
    -- * Field lenses for capability types
  , allowedReadDirs
  , allowedWriteDirs
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Lens (Lens', lens, (^.), (.~), (%~), (&))
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Data.List (isPrefixOf)
import System.Directory (canonicalizePath)
import Data.Time.Clock (UTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Dhall (FromDhall, ToDhall)
import Data.Aeson (FromJSON, ToJSON)

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
instance FromJSON OptimizedName
instance ToJSON OptimizedName

-- | Newtype for original filepath in the repository
newtype OriginalPath = OriginalPath { unOriginalPath :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String
  deriving (Generic)

instance FromDhall OriginalPath
instance ToDhall OriginalPath
instance FromJSON OriginalPath
instance ToJSON OriginalPath

-- | Configuration for the clod program
data ClodConfig = ClodConfig
  { _projectPath    :: !FilePath      -- ^ Root path of the project
  , _stagingDir     :: !FilePath      -- ^ Directory where files will be staged for Claude
  , _configDir      :: !FilePath      -- ^ Directory for configuration files
  , _databaseFile   :: !FilePath      -- ^ Path to the checksums database file
  , _timestamp      :: !String        -- ^ Timestamp for the current run
  , _currentStaging :: !FilePath      -- ^ Path to the current staging directory
  , _previousStaging :: !(Maybe FilePath) -- ^ Path to the previous staging directory, if any
  , _testMode       :: !Bool          -- ^ Whether we're running in test mode
  , _verbose        :: !Bool          -- ^ Whether to print verbose output
  , _flushMode      :: !Bool          -- ^ Whether to flush stale entries from the database
  , _lastMode       :: !Bool          -- ^ Whether to use the previous staging directory
  , _ignorePatterns :: ![IgnorePattern] -- ^ Patterns from .gitignore and .clodignore
  } deriving stock (Show, Eq, Generic)

-- | Lens for projectPath field
projectPath :: Lens' ClodConfig FilePath
projectPath = lens _projectPath (\c v -> c { _projectPath = v })

-- | Lens for stagingDir field
stagingDir :: Lens' ClodConfig FilePath
stagingDir = lens _stagingDir (\c v -> c { _stagingDir = v })

-- | Lens for configDir field
configDir :: Lens' ClodConfig FilePath
configDir = lens _configDir (\c v -> c { _configDir = v })

-- | Lens for databaseFile field
databaseFile :: Lens' ClodConfig FilePath
databaseFile = lens _databaseFile (\c v -> c { _databaseFile = v })

-- | Lens for timestamp field
timestamp :: Lens' ClodConfig String
timestamp = lens _timestamp (\c v -> c { _timestamp = v })

-- | Lens for currentStaging field
currentStaging :: Lens' ClodConfig FilePath
currentStaging = lens _currentStaging (\c v -> c { _currentStaging = v })

-- | Lens for previousStaging field
previousStaging :: Lens' ClodConfig (Maybe FilePath)
previousStaging = lens _previousStaging (\c v -> c { _previousStaging = v })

-- | Lens for testMode field
testMode :: Lens' ClodConfig Bool
testMode = lens _testMode (\c v -> c { _testMode = v })

-- | Lens for verbose field
verbose :: Lens' ClodConfig Bool
verbose = lens _verbose (\c v -> c { _verbose = v })

-- | Lens for flushMode field
flushMode :: Lens' ClodConfig Bool
flushMode = lens _flushMode (\c v -> c { _flushMode = v })

-- | Lens for lastMode field
lastMode :: Lens' ClodConfig Bool
lastMode = lens _lastMode (\c v -> c { _lastMode = v })

-- | Lens for ignorePatterns field
ignorePatterns :: Lens' ClodConfig [IgnorePattern]
ignorePatterns = lens _ignorePatterns (\c v -> c { _ignorePatterns = v })

-- | Result of processing a file
-- 
-- * 'Success' indicates the file was successfully processed and included
-- * 'Skipped' indicates the file was skipped with a reason (matched ignore pattern, binary file, etc.)
data FileResult 
  = Success              -- ^ File was successfully processed
  | Skipped !String      -- ^ File was skipped with the given reason
  deriving stock (Show, Eq, Generic)

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

-- | ClodError prisms are skipped for now
-- Will implement manually if needed

-- | Newtype for file checksums to prevent mixing with other string types
newtype Checksum = Checksum { unChecksum :: String }
  deriving (Show, Eq, Ord) via String
  deriving (IsString, Semigroup, Monoid) via String
  deriving (Generic)

instance FromDhall Checksum
instance ToDhall Checksum
instance FromJSON Checksum
instance ToJSON Checksum

-- | File entry in the checksum database
data FileEntry = FileEntry
  { _entryPath         :: !FilePath       -- ^ Original path
  , _entryChecksum     :: !Checksum       -- ^ File content checksum
  , _entryLastModified :: !UTCTime        -- ^ Last modified time
  , _entryOptimizedName :: !OptimizedName -- ^ Name in staging directory
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall, FromJSON, ToJSON)

-- | Lens for entryPath field
entryPath :: Lens' FileEntry FilePath
entryPath = lens _entryPath (\e v -> e { _entryPath = v })

-- | Lens for entryChecksum field
entryChecksum :: Lens' FileEntry Checksum
entryChecksum = lens _entryChecksum (\e v -> e { _entryChecksum = v })

-- | Lens for entryLastModified field
entryLastModified :: Lens' FileEntry UTCTime
entryLastModified = lens _entryLastModified (\e v -> e { _entryLastModified = v })

-- | Lens for entryOptimizedName field
entryOptimizedName :: Lens' FileEntry OptimizedName
entryOptimizedName = lens _entryOptimizedName (\e v -> e { _entryOptimizedName = v }) 

-- | Main database structure
data ClodDatabase = ClodDatabase
  { _dbFiles          :: !(Map FilePath FileEntry)  -- ^ All tracked files by path
  , _dbChecksums      :: !(Map String FilePath)     -- ^ Mapping from checksum to path (for rename detection)
  , _dbLastStagingDir :: !(Maybe FilePath)          -- ^ Previous staging directory
  , _dbLastRunTime    :: !UTCTime                  -- ^ Time of last run
  } deriving stock (Show, Eq, Generic)

-- | Lens for dbFiles field
dbFiles :: Lens' ClodDatabase (Map FilePath FileEntry)
dbFiles = lens _dbFiles (\d v -> d { _dbFiles = v })

-- | Lens for dbChecksums field
dbChecksums :: Lens' ClodDatabase (Map String FilePath)
dbChecksums = lens _dbChecksums (\d v -> d { _dbChecksums = v })

-- | Lens for dbLastStagingDir field
dbLastStagingDir :: Lens' ClodDatabase (Maybe FilePath)
dbLastStagingDir = lens _dbLastStagingDir (\d v -> d { _dbLastStagingDir = v })

-- | Lens for dbLastRunTime field
dbLastRunTime :: Lens' ClodDatabase UTCTime
dbLastRunTime = lens _dbLastRunTime (\d v -> d { _dbLastRunTime = v })

-- | Serialization-friendly version of ClodDatabase
data SerializableClodDatabase = SerializableClodDatabase
  { serializedFiles          :: ![(FilePath, FileEntry)]  -- ^ All tracked files as pairs
  , serializedChecksums      :: ![(String, FilePath)]     -- ^ Checksums as pairs
  , serializedLastStagingDir :: !(Maybe FilePath)          -- ^ Previous staging directory
  , serializedLastRunTime    :: !UTCTime                  -- ^ Time of last run
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall, FromJSON, ToJSON)

-- | Convert to serializable form
toSerializable :: ClodDatabase -> SerializableClodDatabase
toSerializable db = SerializableClodDatabase
  { serializedFiles = Map.toList (db ^. dbFiles)
  , serializedChecksums = Map.toList (db ^. dbChecksums)
  , serializedLastStagingDir = db ^. dbLastStagingDir
  , serializedLastRunTime = db ^. dbLastRunTime
  }

-- | Convert from serializable form
fromSerializable :: SerializableClodDatabase -> ClodDatabase
fromSerializable sdb = ClodDatabase
  { _dbFiles = Map.fromList (serializedFiles sdb)
  , _dbChecksums = Map.fromList (serializedChecksums sdb)
  , _dbLastStagingDir = serializedLastStagingDir sdb
  , _dbLastRunTime = serializedLastRunTime sdb
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
  { _allowedReadDirs :: [FilePath] -- ^ Directories where reading is permitted
  } deriving (Show, Eq)

-- | Lens for allowedReadDirs field
allowedReadDirs :: Lens' FileReadCap [FilePath]
allowedReadDirs = lens _allowedReadDirs (\c v -> c { _allowedReadDirs = v })

-- | Capability for writing files within certain directories
data FileWriteCap = FileWriteCap 
  { _allowedWriteDirs :: [FilePath] -- ^ Directories where writing is permitted
  } deriving (Show, Eq)

-- | Lens for allowedWriteDirs field
allowedWriteDirs :: Lens' FileWriteCap [FilePath]
allowedWriteDirs = lens _allowedWriteDirs (\c v -> c { _allowedWriteDirs = v })

-- | Create a file read capability for specified directories
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap dirs = FileReadCap { _allowedReadDirs = dirs }

-- | Create a file write capability for specified directories
fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap dirs = FileWriteCap { _allowedWriteDirs = dirs }

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