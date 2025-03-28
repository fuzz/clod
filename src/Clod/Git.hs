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
  
    -- * File processing
  , processModifiedFiles
  , processAllFiles
  ) where

-- Import from our libgit2-based implementation
import Clod.Git.LibGit (getRepositoryRoot, isGitRepository, checkUncommittedChanges,
                        getUntrackedFiles, getModifiedFiles)

-- Re-export file processing functions from Git.Internal
import Clod.Git.Internal (processModifiedFiles, processAllFiles)
