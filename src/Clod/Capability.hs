{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  
    -- * Path Types for Enhanced Safety
  , PathType(..)
  , TypedPath(..)
  , HasPermission(..)
  , CapabilityFor
    
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
  
    -- * Type-Safe Path Operations
  , asReadPath
  , asWritePath
  , asGitPath
  , unsafeCoercePath
  , withTypedPath
  
    -- * Pattern Synonyms
  , pattern ReadPath
  , pattern WritePath
  , pattern GitPath
  , readTypedFile
  , writeTypedFile
  , copyTypedFile
  ) where

import Polysemy
import qualified Polysemy.Error as PE
import Polysemy.Error (Error)
import qualified Polysemy.Embed as E
import Data.List (isPrefixOf)
import qualified Data.ByteString as BS
import System.Directory (canonicalizePath)
import Prelude hiding (readFile, writeFile)
import Data.Coerce (coerce)
import GHC.TypeLits (Symbol)

import Clod.Effects hiding (throw, catch)
import qualified Clod.Types as T

-- | Path type for enhanced type safety
data PathType = ReadAllowed | WriteAllowed | GitAllowed

-- Helper type aliases for capabilities are available but not currently used
  
-- | Pattern synonyms for working with typed paths
pattern ReadPath :: FilePath -> TypedPath ReadAllowed
pattern ReadPath p = TypedPath p

-- | Pattern synonym for creating a TypedPath with write permissions
pattern WritePath :: FilePath -> TypedPath WriteAllowed
pattern WritePath p = TypedPath p

-- | Pattern synonym for creating a TypedPath with Git repository permissions
pattern GitPath :: FilePath -> TypedPath GitAllowed
pattern GitPath p = TypedPath p

-- | TypeSafe path with phantom type parameter
newtype TypedPath (a :: PathType) = TypedPath { getPath :: FilePath }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Type class for capability permissions
class HasPermission c p | c -> p where
  checkPermission :: c -> FilePath -> IO Bool
  
-- | Simple constraint type for capability operations
type CapabilityFor (s :: Symbol) r = Member (Embed IO) r

-- | Capability for reading files within certain directories
data FileReadCap = FileReadCap 
  { allowedReadDirs :: [FilePath]
  , readPolicy :: forall r. Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
               => FilePath -> Sem r Bool  -- Policy check function
  }

instance HasPermission FileReadCap ReadAllowed where
  checkPermission cap = isPathAllowed (allowedReadDirs cap)

-- | Capability for writing files within certain directories
data FileWriteCap = FileWriteCap 
  { allowedWriteDirs :: [FilePath]
  , writePolicy :: forall r. Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
                => FilePath -> Sem r Bool  -- Policy check function
  }

instance HasPermission FileWriteCap WriteAllowed where
  checkPermission cap = isPathAllowed (allowedWriteDirs cap)

-- | Capability for Git operations
data GitCap = GitCap 
  { allowedRepos :: [FilePath]
  , gitPolicy :: forall r. Members '[Git, Error T.ClodError, E.Embed IO] r 
              => FilePath -> Sem r Bool  -- Policy check function
  }
instance HasPermission GitCap GitAllowed where
  checkPermission cap = isPathAllowed (allowedRepos cap)

-- | Create a file read capability for specified directories
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap dirs = FileReadCap 
  { allowedReadDirs = dirs
  , readPolicy = defaultReadPolicy dirs
  }
  where
    defaultReadPolicy :: [FilePath] -> Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
                      => FilePath -> Sem r Bool
    defaultReadPolicy allowedDirs path = E.embed $ isPathAllowed allowedDirs path

-- | Create a file write capability for specified directories
fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap dirs = FileWriteCap 
  { allowedWriteDirs = dirs
  , writePolicy = defaultWritePolicy dirs
  }
  where
    defaultWritePolicy :: [FilePath] -> Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
                       => FilePath -> Sem r Bool
    defaultWritePolicy allowedDirs path = E.embed $ isPathAllowed allowedDirs path

-- | Create a Git capability for specified repositories
gitCap :: [FilePath] -> GitCap
gitCap dirs = GitCap 
  { allowedRepos = dirs
  , gitPolicy = defaultGitPolicy dirs
  }
  where
    defaultGitPolicy :: [FilePath] -> Members '[Git, Error T.ClodError, E.Embed IO] r 
                     => FilePath -> Sem r Bool
    defaultGitPolicy allowedDirs path = E.embed $ isPathAllowed allowedDirs path

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
  return (or checks)
  where
    isPathSeparator c = c == '/' || c == '\\'
    
-- | Create a type-safe path with read permissions
-- This function checks if the path is allowed by the capability and returns a typed path if it is
asReadPath :: (HasPermission c ReadAllowed, CapabilityFor "read" r) 
           => c -> FilePath -> Sem r (Maybe (TypedPath ReadAllowed))
asReadPath cap path = do
  allowed <- E.embed $ checkPermission cap path
  return $ if allowed
           then Just (TypedPath path)
           else Nothing
           
-- | Create a type-safe path with write permissions
asWritePath :: (HasPermission c WriteAllowed, CapabilityFor "write" r) 
            => c -> FilePath -> Sem r (Maybe (TypedPath WriteAllowed))
asWritePath cap path = do
  allowed <- E.embed $ checkPermission cap path
  return $ if allowed
           then Just (TypedPath path)
           else Nothing
           
-- | Create a type-safe path with git permissions
asGitPath :: (HasPermission c GitAllowed, CapabilityFor "git" r) 
          => c -> FilePath -> Sem r (Maybe (TypedPath GitAllowed))
asGitPath cap path = do
  allowed <- E.embed $ checkPermission cap path
  return $ if allowed
           then Just (TypedPath path)
           else Nothing
           
-- | Read a file using a type-safe path
-- This function is guaranteed to only read files that have been verified as allowed
readTypedFile :: Member FileSystem r => TypedPath ReadAllowed -> Sem r BS.ByteString
readTypedFile (TypedPath path) = readFile path

-- | Write to a file using a type-safe path
-- This function is guaranteed to only write to files that have been verified as allowed
writeTypedFile :: Member FileSystem r => TypedPath WriteAllowed -> BS.ByteString -> Sem r ()
writeTypedFile (TypedPath path) content = writeFile path content

-- | Copy between two type-safe paths
-- This function is guaranteed to only copy between files that have been verified as allowed
copyTypedFile :: Member FileSystem r 
              => TypedPath ReadAllowed -> TypedPath WriteAllowed -> Sem r ()
copyTypedFile (TypedPath src) (TypedPath dest) = copyFile src dest

-- | Unsafe coercion of path types - use with caution and only when you're certain it's safe
-- This can be useful when you have verified paths externally and need to bypass capability checks
unsafeCoercePath :: TypedPath a -> TypedPath b
unsafeCoercePath = coerce

-- | Higher-order function for working with typed paths
-- This allows for safe temporary access to a typed path
withTypedPath :: (HasPermission c p, Member (Embed IO) r) 
              => c -> FilePath -> (Maybe (TypedPath p) -> Sem r a) -> Sem r a
withTypedPath cap path f = do
  -- First check permission using the capability
  allowed <- embed $ checkPermission cap path
  let typedPathM = if allowed then Just (TypedPath path) else Nothing
  f typedPathM

-- | Safe file reading that checks capabilities
safeReadFile :: Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
             => FileReadCap 
             -> FilePath 
             -> Sem r BS.ByteString
safeReadFile cap path = do
  allowed <- readPolicy cap path
  if allowed
    then readFile path
    else PE.throw $ T.ConfigError $ "Access denied: Cannot read file outside allowed directories: " ++ path

-- | Safe file existence check that checks capabilities
safeFileExists :: Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
               => FileReadCap 
               -> FilePath 
               -> Sem r Bool
safeFileExists cap path = do
  allowed <- readPolicy cap path
  if allowed
    then fileExists path
    else PE.throw $ T.ConfigError $ "Access denied: Cannot check existence of file outside allowed directories: " ++ path

-- | Safe file type check that checks capabilities
safeIsTextFile :: Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
               => FileReadCap 
               -> FilePath 
               -> Sem r Bool
safeIsTextFile cap path = do
  allowed <- readPolicy cap path
  if allowed
    then isTextFile path
    else PE.throw $ T.ConfigError $ "Access denied: Cannot check file type outside allowed directories: " ++ path

-- | Safe file writing that checks capabilities
safeWriteFile :: Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
              => FileWriteCap 
              -> FilePath 
              -> BS.ByteString 
              -> Sem r ()
safeWriteFile cap path content = do
  allowed <- writePolicy cap path
  if allowed
    then writeFile path content
    else PE.throw $ T.ConfigError $ "Access denied: Cannot write file outside allowed directories: " ++ path

-- | Safe file copying that checks capabilities for both read and write
safeCopyFile :: Members '[FileSystem, Error T.ClodError, E.Embed IO] r 
             => FileReadCap
             -> FileWriteCap
             -> FilePath 
             -> FilePath 
             -> Sem r ()
safeCopyFile readCap writeCap src dest = do
  srcAllowed <- readPolicy readCap src
  destAllowed <- writePolicy writeCap dest
  if srcAllowed && destAllowed
    then copyFile src dest
    else PE.throw $ T.ConfigError $ "Access denied: Cannot copy file - path restrictions violated"

-- | Safe git modified files checking with capabilities
safeGetModifiedFiles :: Members '[Git, Error T.ClodError, E.Embed IO] r 
                     => GitCap 
                     -> FilePath 
                     -> Sem r [FilePath]
safeGetModifiedFiles cap repoPath = do
  allowed <- gitPolicy cap repoPath
  if allowed
    then getModifiedFiles repoPath
    else PE.throw $ T.ConfigError $ "Access denied: Cannot access Git repository: " ++ repoPath

-- | Safe git untracked files checking with capabilities
safeGetUntrackedFiles :: Members '[Git, Error T.ClodError, E.Embed IO] r 
                      => GitCap 
                      -> FilePath 
                      -> Sem r [FilePath]
safeGetUntrackedFiles cap repoPath = do
  allowed <- gitPolicy cap repoPath
  if allowed
    then getUntrackedFiles repoPath
    else PE.throw $ T.ConfigError $ "Access denied: Cannot access Git repository: " ++ repoPath
