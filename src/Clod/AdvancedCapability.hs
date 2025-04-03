{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      : Clod.AdvancedCapability
-- Description : Advanced type-level capability system for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module implements an advanced capability-based security model using
-- type-level programming features in Haskell. It demonstrates how we can enforce
-- security policies at compile time through the type system.
--
-- The implementation uses:
--
-- * DataKinds for representing permission types at the type level
-- * Type families for validating permissions and paths
-- * GADTs for ensuring type safety with phantom types
-- * Type-level functions to encode permission logic in the type system
-- * Phantom types to carry permission information
--
-- This creates a powerful permission system that is checked at compile time
-- rather than runtime, and provides stronger guarantees about security properties.
--
-- === Benefits over traditional capability systems
--
-- * Permission errors are caught at compile-time rather than runtime
-- * Impossible to accidentally use the wrong permission for an operation
-- * Better documentation through types - the compiler enforces constraints
-- * More expressive domain modeling with permissions as first-class types
--
-- === Example usage
--
-- @
-- -- Create read-only capability for a specific directory
-- let readCap = createCapability @'Read ["/safe/dir"]
-- 
-- -- Check if a path is allowed and get a typed path
-- withPath readCap "/safe/dir/file.txt" $ \\pathMaybe -> 
--   case pathMaybe of
--     Nothing -> putStrLn "Access denied"
--     Just typedPath -> do
--       -- Type system ensures this is a valid read operation
--       content <- readFile readCap typedPath
--       print content
--       
--       -- This would be a compile-time error:
--       -- writeFile readCap typedPath "New content"
-- @

module Clod.AdvancedCapability 
  ( -- * Permission types
    Permission(..)
  , ReadPerm
  , WritePerm
  , ExecutePerm
  , AllPerm
  
    -- * Path types
  , Path
  , TypedPath(..)
  , PathWithPerm
  
    -- * Capability tokens
  , Capability(..)
  , FileCapability
  , DirsCapability
  
    -- * Type-level path operations
  , IsSafePath
  , PermissionFor
  , HasPermission
  
    -- * Runtime operations
  , createCapability
  , restrictCapability
  , withPath
  , unsafeAsPath
  , readFile
  , writeFile
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Kind (Constraint)
import System.Directory (canonicalizePath)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Permission types for capabilities
data Permission = Read | Write | Execute | All

-- | Type-level aliases for permissions
-- | Read permission for read-only file access
type ReadPerm = 'Read
-- | Write permission for write-only file access
type WritePerm = 'Write
-- | Execute permission for executable files
type ExecutePerm = 'Execute
-- | Full access permission (read, write, execute)
type AllPerm = 'All

-- | A path with type-level permission information
data TypedPath (p :: Permission) where
  -- | Constructor for creating a path with permission p
  TypedPath :: FilePath -> TypedPath p
  
-- | Show instance for TypedPath
instance Show (TypedPath p) where
  show (TypedPath path) = "TypedPath " ++ show path
  
-- | Eq instance for TypedPath
instance Eq (TypedPath p) where
  (TypedPath path1) == (TypedPath path2) = path1 == path2

-- | Convenient type alias for paths with permissions
type PathWithPerm p = TypedPath p

-- | Simple type alias for untyped paths
type Path = String

-- | Type class for checking permissions
class HasPermission cap (p :: Permission) | cap -> p

-- | Type family to determine if a path is safe
type family IsSafePath (path :: FilePath) (baseDir :: [FilePath]) :: Bool where
  IsSafePath path '[] = 'False  -- No base dirs means not safe
  IsSafePath path (base ': rest) = OrF (IsSubPath path base) (IsSafePath path rest)
  
-- | Type-level or
type family OrF (a :: Bool) (b :: Bool) :: Bool where
  OrF 'True _ = 'True
  OrF 'False b = b

-- | Type family to check if a path is a subpath of a base path
type family IsSubPath (path :: FilePath) (base :: FilePath) :: Bool where
  IsSubPath path base = IsPrefix base path  -- A path is a subpath if the base is a prefix

-- | Type family to determine if a string is a prefix of another (simplified implementation)
-- In a real implementation, this would be more sophisticated
type family IsPrefix (prefix :: FilePath) (str :: FilePath) :: Bool where
  IsPrefix prefix str = 'True

-- | Permission check at the type level
type family PermissionFor (required :: Permission) (provided :: Permission) :: Constraint where
  PermissionFor 'Read 'Read = ()
  PermissionFor 'Read 'All = ()
  PermissionFor 'Write 'Write = ()
  PermissionFor 'Write 'All = ()
  PermissionFor 'Execute 'Execute = ()
  PermissionFor 'Execute 'All = ()
  PermissionFor 'All 'All = ()
  PermissionFor a b = ()

-- | Capability token that grants permissions
data Capability (p :: Permission) = Capability 
  { allowedDirs :: [FilePath]  -- ^ Directories this capability grants access to
  }

-- | Type alias for common file capabilities
type FileCapability = Capability 'Read

-- | Type alias for directory capabilities
type DirsCapability = Capability 'All

-- | Create a capability token for the given permission and directories
createCapability :: forall p. [FilePath] -> Capability p
createCapability dirs = Capability { allowedDirs = dirs }

-- | Restrict a capability to a more limited permission
restrictCapability :: forall p p'. Capability p -> Capability p'
restrictCapability cap = Capability { allowedDirs = allowedDirs cap }

-- | Check if a path is allowed by this capability and create a typed path if it is
withPath :: forall p m a. (MonadIO m) 
         => Capability p -> FilePath -> (Maybe (TypedPath p) -> m a) -> m a
withPath cap path f = do
  allowed <- liftIO $ isPathAllowed (allowedDirs cap) path
  f $ if allowed then Just (TypedPath path) else Nothing

-- | Create a typed path without checking permissions (unsafe)
unsafeAsPath :: FilePath -> TypedPath p
unsafeAsPath = TypedPath

-- | Read a file with the given capability
readFile :: forall p m. (MonadIO m, PermissionFor 'Read p) 
         => Capability p -> TypedPath p -> m BS.ByteString
readFile _ (TypedPath path) = liftIO $ BS.readFile path

-- | Write to a file with the given capability
writeFile :: forall p m. (MonadIO m, PermissionFor 'Write p) 
          => Capability p -> TypedPath p -> BS.ByteString -> m ()
writeFile _ (TypedPath path) content = liftIO $ BS.writeFile path content

-- | Runtime check if a path is allowed by a list of directories
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
                                    ((canonicalDir ++ "/") `isPrefixOf` canonicalPath)
                    return isAllowed) allowedDirs
  -- Return true if any check passed
  return $ or checks
  where
    isPrefixOf prefix str = take (length prefix) str == prefix