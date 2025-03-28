{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clod.Capability
-- Description : Capability-based security for AI operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module implements a capability-based security model for
-- AI-assisted operations. It ensures that AI-generated code can only
-- perform permitted operations on specified files and directories.
--
-- === Capability-Based Security
--
-- Capability-based security is a powerful approach to restricting what operations are
-- allowed in AI-generated code. A capability is essentially a token that grants permission
-- to perform a specific operation. In our implementation, capabilities are type-safe
-- representations of permissions to access specific directories.
--
-- === How It Works
--
-- When code wants to perform a file operation, it must prove it has permission
-- by providing a capability that encompasses the path. Instead of calling the raw
-- file operation directly, it must use the "safe" version that checks the capability.
--
-- For example:
--
-- @
-- -- This won't compile because readFile is hidden
-- content <- readFile "/etc/passwd"
--
-- -- This uses the safe version but will fail at runtime if the path isn't allowed
-- content <- safeReadFile myReadCap "/etc/passwd"
--
-- -- This will succeed because the path is specifically allowed
-- content <- safeReadFile (fileReadCap ["/home/user/projects"]) "/home/user/projects/myfile.txt"
-- @
--
-- === Benefits for AI Safety
--
-- 1. **Path Restriction**: AI can only access files in explicitly permitted directories
-- 2. **Type Safety**: Capabilities are enforced by the type system
-- 3. **Intent Signaling**: Functions that need file access must explicitly request capabilities
-- 4. **Principle of Least Privilege**: AI code only gets the minimal permissions it needs
--
-- This approach is particularly valuable for AI-generated code because it prevents
-- even buggy or malicious code from accessing sensitive parts of the filesystem.

module Clod.Capability 
  ( -- * Capability Types
    FileReadCap(..)
  , FileWriteCap(..)
  , GitCap(..)
    
    -- * Capability Creation
  , fileReadCap
  , fileWriteCap
  , gitCap
    
    -- * Safe Operations
  , safeReadFile
  , safeWriteFile
  , safeCopyFile
  , safeFileExists
  , safeIsTextFile
  , safeGetModifiedFiles
  , safeGetUntrackedFiles
  ) where

import Polysemy
import Polysemy.Error
import Data.List (isPrefixOf)
import qualified Data.ByteString as BS
import Prelude hiding (readFile, writeFile)

import Clod.Effects
import qualified Clod.Types as T

-- | Capability for reading files within certain directories
newtype FileReadCap = FileReadCap { allowedReadDirs :: [FilePath] }

-- | Capability for writing files within certain directories
newtype FileWriteCap = FileWriteCap { allowedWriteDirs :: [FilePath] }

-- | Capability for Git operations
newtype GitCap = GitCap { allowedRepos :: [FilePath] }

-- | Create a file read capability for specified directories
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap = FileReadCap

-- | Create a file write capability for specified directories
fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap = FileWriteCap

-- | Create a Git capability for specified repositories
gitCap :: [FilePath] -> GitCap
gitCap = GitCap

-- | Check if a path is within allowed directories
isPathAllowed :: [FilePath] -> FilePath -> Bool
isPathAllowed allowedDirs path = any (\dir -> dir `isPrefixOf` path) allowedDirs

-- | Safe file reading that checks capabilities
safeReadFile :: Members '[FileSystem, Error T.ClodError] r 
             => FileReadCap 
             -> FilePath 
             -> Sem r BS.ByteString
safeReadFile cap path = 
  if isPathAllowed (allowedReadDirs cap) path
    then readFile path
    else throw $ T.ConfigError $ "Access denied: Cannot read file outside allowed directories: " ++ path

-- | Safe file existence check that checks capabilities
safeFileExists :: Members '[FileSystem, Error T.ClodError] r 
               => FileReadCap 
               -> FilePath 
               -> Sem r Bool
safeFileExists cap path = 
  if isPathAllowed (allowedReadDirs cap) path
    then fileExists path
    else throw $ T.ConfigError $ "Access denied: Cannot check existence of file outside allowed directories: " ++ path

-- | Safe file type check that checks capabilities
safeIsTextFile :: Members '[FileSystem, Error T.ClodError] r 
               => FileReadCap 
               -> FilePath 
               -> Sem r Bool
safeIsTextFile cap path = 
  if isPathAllowed (allowedReadDirs cap) path
    then isTextFile path
    else throw $ T.ConfigError $ "Access denied: Cannot check file type outside allowed directories: " ++ path

-- | Safe file writing that checks capabilities
safeWriteFile :: Members '[FileSystem, Error T.ClodError] r 
              => FileWriteCap 
              -> FilePath 
              -> BS.ByteString 
              -> Sem r ()
safeWriteFile cap path content = 
  if isPathAllowed (allowedWriteDirs cap) path
    then writeFile path content
    else throw $ T.ConfigError $ "Access denied: Cannot write file outside allowed directories: " ++ path

-- | Safe file copying that checks capabilities for both read and write
safeCopyFile :: Members '[FileSystem, Error T.ClodError] r 
             => FileReadCap
             -> FileWriteCap
             -> FilePath 
             -> FilePath 
             -> Sem r ()
safeCopyFile readCap writeCap src dest = 
  if isPathAllowed (allowedReadDirs readCap) src && isPathAllowed (allowedWriteDirs writeCap) dest
    then copyFile src dest
    else throw $ T.ConfigError $ "Access denied: Cannot copy file - path restrictions violated"

-- | Safe git modified files checking with capabilities
safeGetModifiedFiles :: Members '[Git, Error T.ClodError] r 
                     => GitCap 
                     -> FilePath 
                     -> Sem r [FilePath]
safeGetModifiedFiles cap repoPath = 
  if isPathAllowed (allowedRepos cap) repoPath
    then getModifiedFiles repoPath
    else throw $ T.ConfigError $ "Access denied: Cannot access Git repository: " ++ repoPath

-- | Safe git untracked files checking with capabilities
safeGetUntrackedFiles :: Members '[Git, Error T.ClodError] r 
                      => GitCap 
                      -> FilePath 
                      -> Sem r [FilePath]
safeGetUntrackedFiles cap repoPath = 
  if isPathAllowed (allowedRepos cap) repoPath
    then getUntrackedFiles repoPath
    else throw $ T.ConfigError $ "Access denied: Cannot access Git repository: " ++ repoPath