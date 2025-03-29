{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- |
-- Module      : Clod.Effects
-- Description : Effect system for Clod operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module defines the effect system for the Clod application,
-- providing safe, typed effects for file operations, Git interactions,
-- and other side effects. This ensures that AI-assisted code generation
-- can only perform permitted operations and helps prevent errors.
--
-- === AI Safety
-- 
-- The effect system acts as a safety mechanism for AI-generated code. When code is generated
-- by an AI system, there's always a risk of unintended behavior, especially when it comes
-- to operations that could modify files or access sensitive data. This system uses algebraic
-- effects to:
--
-- 1. Make effects explicit in type signatures, so it's clear what side effects a function can have
-- 2. Separate effect definition from effect implementation, allowing for different interpreters
-- 3. Provide a capability-based security model to restrict operations to specific directories/files
--
-- === Example: Safe File Processing with AI
--
-- Here's how an AI-generated file processing function would be constrained by this system:
--
-- @
-- processFilesGeneratedByAI :: Members '[FileSystem, Error ClodError, Console] r
--                           => FileReadCap -> FileWriteCap -> [FilePath] -> Sem r ()
-- processFilesGeneratedByAI readCap writeCap files = do
--   -- AI can only read files within directories specified by readCap
--   -- AI can only write files within directories specified by writeCap
--   -- AI cannot perform unconstrained IO
--   forM_ files $ \file -> do
--     exists <- safeFileExists readCap file
--     when exists $ do
--       -- Content is restricted to specified paths
--       content <- safeReadFile readCap file
--       -- Processing can happen freely as it's pure
--       let processed = process content
--       -- Writing is restricted to specified paths
--       safeWriteFile writeCap (file ++ ".processed") processed
-- @
--
-- Even if the AI generates incorrect or malicious code, the capability-based system
-- prevents it from accessing files outside the specified directories.

module Clod.Effects 
  ( -- * File System Effects
    FileSystem(..)
  , readFile
  , writeFile
  , copyFile
  , fileExists
  , isTextFile
  , runFileSystemIO
    
    -- * Git Effects
  , Git(..)
  , getModifiedFiles
  , getUntrackedFiles
  , runGitIO
    
    -- * Console Effects
  , Console(..)
  , logInfo
  , logWarning
  , logError
  , logOutput
  , runConsoleIO
    
    -- * Error Handling
    -- | Throw an error in an effect context
    , throw
    -- | Catch an error in an effect context
    , catch

    -- | Documentation for generated functions (not exported)
    -- These are only used to provide proper Haddock documentation
    -- for the functions generated by makeSem.
    -- @
    -- _readFileDoc
    -- _writeFileDoc 
    -- _copyFileDoc
    -- _fileExistsDoc
    -- _isTextFileDoc
    -- _getModifiedFilesDoc
    -- _getUntrackedFilesDoc
    -- _logInfoDoc
    -- _logWarningDoc
    -- _logErrorDoc
    -- _logOutputDoc
    -- @
    
    -- * Type-Level Constraints and Utilities
    , EffectStack
    , type (+>)
    , CapabilityConstraint
    , PathRestricted
    
    -- * Running Effects
    , runClodEffects
  ) where

import Polysemy
import qualified Polysemy.Error as PE
import Polysemy.Error (Error)
import Polysemy.Reader
import qualified System.Directory as Dir
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import System.Process (readProcess)
import Prelude hiding (readFile, writeFile)
import qualified Clod.Git.LibGit as GitLib
import qualified Clod.Types as T
import qualified Control.Exception as Exception
import Control.Exception (SomeException)
import Data.Function ((&))
import GHC.Exts (Constraint)

-- | Throw an error in an effect context
-- Re-exported from Polysemy.Error
throw :: Member (Error e) r => e -> Sem r a
throw = PE.throw

-- | Catch an error in an effect context
-- Re-exported from Polysemy.Error
catch :: Member (Error e) r => Sem r a -> (e -> Sem r a) -> Sem r a
catch = PE.catch

-- | Type-level operator for combining effect constraints
type family (+>) (a :: Constraint) (b :: Constraint) :: Constraint where
  a +> b = (a, b)

-- | Effect for file system operations
data FileSystem m a where
  -- | Read file content from a path
  ReadFile :: FilePath -> FileSystem m BS.ByteString
  -- | Write content to a file path
  WriteFile :: FilePath -> BS.ByteString -> FileSystem m ()
  -- | Copy a file from source to destination
  CopyFile :: FilePath -> FilePath -> FileSystem m ()
  -- | Check if a file exists at the given path
  FileExists :: FilePath -> FileSystem m Bool
  -- | Check if a file is a text file (not binary)
  IsTextFile :: FilePath -> FileSystem m Bool

-- Generate effect functions with Template Haskell
-- Generate effect functions with Template Haskell
makeSem ''FileSystem

-- | Read a file from the file system
-- Re-exported from FileSystem effect
_readFileDoc :: Member FileSystem r => FilePath -> Sem r BS.ByteString
_readFileDoc = readFile

-- | Write content to a file
-- Re-exported from FileSystem effect
_writeFileDoc :: Member FileSystem r => FilePath -> BS.ByteString -> Sem r ()
_writeFileDoc = writeFile

-- | Copy a file from one path to another
-- Re-exported from FileSystem effect
_copyFileDoc :: Member FileSystem r => FilePath -> FilePath -> Sem r ()
_copyFileDoc = copyFile

-- | Check if a file exists
-- Re-exported from FileSystem effect
_fileExistsDoc :: Member FileSystem r => FilePath -> Sem r Bool
_fileExistsDoc = fileExists

-- | Check if a file is a text file
-- Re-exported from FileSystem effect
_isTextFileDoc :: Member FileSystem r => FilePath -> Sem r Bool
_isTextFileDoc = isTextFile

-- | Effect for Git operations
data Git m a where
  -- | Get list of modified files in a Git repository
  GetModifiedFiles :: FilePath -> Git m [FilePath]
  -- | Get list of untracked files in a Git repository
  GetUntrackedFiles :: FilePath -> Git m [FilePath]

-- Generate Git effect functions
makeSem ''Git

-- | Get modified files from a Git repository
-- Re-exported from Git effect
_getModifiedFilesDoc :: Member Git r => FilePath -> Sem r [FilePath]
_getModifiedFilesDoc = getModifiedFiles

-- | Get untracked files from a Git repository
-- Re-exported from Git effect
_getUntrackedFilesDoc :: Member Git r => FilePath -> Sem r [FilePath]
_getUntrackedFilesDoc = getUntrackedFiles

-- | Effect for console output
data Console m a where
  -- | Log informational message
  LogInfo :: String -> Console m ()
  -- | Log warning message
  LogWarning :: String -> Console m ()
  -- | Log error message
  LogError :: String -> Console m ()
  -- | Output message to stdout (for piping to other programs)
  LogOutput :: String -> Console m ()

-- Generate Console effect functions
makeSem ''Console

-- | Log informational message to console
-- Re-exported from Console effect
_logInfoDoc :: Member Console r => String -> Sem r ()
_logInfoDoc = logInfo

-- | Log warning message to console
-- Re-exported from Console effect
_logWarningDoc :: Member Console r => String -> Sem r ()
_logWarningDoc = logWarning

-- | Log error message to console
-- Re-exported from Console effect
_logErrorDoc :: Member Console r => String -> Sem r ()
_logErrorDoc = logError

-- | Log output message to standard output (for piping to other programs)
-- Re-exported from Console effect
_logOutputDoc :: Member Console r => String -> Sem r ()
_logOutputDoc = logOutput

-- | Standard effect stack for most operations
type EffectStack r = 
  ( Member FileSystem r
  , Member Git r
  , Member Console r
  , Member (Error T.ClodError) r
  , Member (Reader T.ClodConfig) r
  , Member (Embed IO) r
  )

-- | Type synonym for a common effect stack with capability constraints
type CapabilityConstraint c r = 
  (Member (Reader c) r, Member (Error T.ClodError) r, Member (Embed IO) r)
  
-- | The PathRestricted effect constraint for operations with path-based capabilities
type PathRestricted c r =
  (Member FileSystem r, Member (Reader c) r, Member (Error T.ClodError) r, Member (Embed IO) r)

-- | Run all Clod effects to IO
runClodEffects :: T.ClodConfig 
               -> Sem '[FileSystem, Git, Console, Error T.ClodError, Reader T.ClodConfig, Embed IO] a 
               -> IO (Either T.ClodError a)
runClodEffects config effect = effect
  & runFileSystemIO
  & runGitIO
  & runConsoleIO
  & PE.runError
  & runReader config
  & runM

-- | Run file system operations in IO
runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  ReadFile path -> embed $ BS.readFile path
  WriteFile path content -> embed $ BS.writeFile path content
  CopyFile src dest -> embed $ Dir.copyFile src dest
  FileExists path -> embed $ Dir.doesFileExist path
  IsTextFile path -> embed $ do
    -- Reimplement the isTextFile logic from Detection module
    result <- Exception.try (readProcess "file" ["--mime-type", "-b", path] "") :: IO (Either SomeException String)
    case result of
      Left _ -> return False  -- On error, assume it's not a text file
      Right mimeType -> return $ "text/" `isPrefixOf` mimeType || 
                                "application/json" `isPrefixOf` mimeType ||
                                "application/xml" `isPrefixOf` mimeType

-- | Run Git operations in IO
runGitIO :: Members '[Error T.ClodError, Embed IO] r => Sem (Git ': r) a -> Sem r a
runGitIO = interpret $ \case
  GetModifiedFiles path -> do
    -- Call GitLib functions directly without using ClodM
    result <- embed $ Exception.try $ GitLib.directGetModifiedFiles path
    case result of
      Left (e :: SomeException) -> PE.throw $ T.GitError $ "Error getting modified files: " ++ show e
      Right files -> pure files
  GetUntrackedFiles path -> do
    -- Call GitLib functions directly without using ClodM
    result <- embed $ Exception.try $ GitLib.directGetUntrackedFiles path
    case result of
      Left (e :: SomeException) -> PE.throw $ T.GitError $ "Error getting untracked files: " ++ show e
      Right files -> pure files

-- | Run console operations in IO
runConsoleIO :: Member (Embed IO) r => Sem (Console ': r) a -> Sem r a
runConsoleIO = interpret $ \case
  LogInfo msg -> embed $ putStrLn msg
  LogWarning msg -> embed $ putStrLn $ "Warning: " ++ msg
  LogError msg -> embed $ putStrLn $ "Error: " ++ msg
  LogOutput msg -> embed $ putStrLn msg  -- Clean output for piping