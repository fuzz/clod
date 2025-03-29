{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Git
-- Description : Git integration for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides the main interface for Git operations in Clod.
-- It re-exports the public functions from the Git submodules.
--
-- The module integrates with the libgit2 library (through hlibgit2 bindings) to:
--
-- * Locate the repository root directory
-- * Check for uncommitted changes
-- * Find files that have been modified since a previous run
-- * Discover untracked files
-- * Process modified or all files for Claude integration

module Clod.Git
  ( -- * Repository operations
    getRepositoryRoot
  , isGitRepository
  
    -- * Status operations
  , checkUncommittedChanges
  , getUntrackedFiles
  , getModifiedFiles
  
    -- * Safe git operations with capabilities
  , safeGetModifiedFiles
  , safeGetUntrackedFiles
  
    -- * File processing
  , processModifiedFiles
  , processAllFiles
  ) where

-- Import from our libgit2-based implementation
import Clod.Git.LibGit (getRepositoryRoot, isGitRepository, checkUncommittedChanges,
                        getUntrackedFiles, getModifiedFiles)

-- Re-export file processing functions from Git.Internal
import Clod.Git.Internal (processModifiedFiles, processAllFiles)

import Clod.Types (ClodM, GitCap(..), ClodError(..), isPathAllowed)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Directory (canonicalizePath)

-- | Safe git modified files checking with capabilities
safeGetModifiedFiles :: GitCap -> FilePath -> ClodM [FilePath]
safeGetModifiedFiles cap repoPath = do
  allowed <- liftIO $ isPathAllowed (allowedRepos cap) repoPath
  if allowed
    then getModifiedFiles repoPath
    else do
      canonicalPath <- liftIO $ canonicalizePath repoPath
      throwError $ CapabilityError $ "Access denied: Cannot access Git repository: " ++ canonicalPath

-- | Safe git untracked files checking with capabilities
safeGetUntrackedFiles :: GitCap -> FilePath -> ClodM [FilePath]
safeGetUntrackedFiles cap repoPath = do
  allowed <- liftIO $ isPathAllowed (allowedRepos cap) repoPath
  if allowed
    then getUntrackedFiles repoPath
    else do
      canonicalPath <- liftIO $ canonicalizePath repoPath
      throwError $ CapabilityError $ "Access denied: Cannot access Git repository: " ++ canonicalPath