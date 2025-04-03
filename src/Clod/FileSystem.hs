{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem
-- Description : File system operations for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module provides functionality for working with files and directories,
-- including finding, reading, copying, and checking files.
--
-- The module handles various file system tasks:
--
-- * Recursively finding files in a directory structure
-- * Detecting modified files since a given timestamp
-- * Identifying text vs. binary files
-- * Processing files according to ignore patterns
-- * Creating an optimized file structure for Claude AI integration
--
-- === File Processing Pipeline
--
-- 1. Files are discovered recursively in the repository
-- 2. Each file is checked against .gitignore and .clodignore patterns
-- 3. Binary files are excluded
-- 4. Remaining files are copied to a staging directory with optimized names
-- 5. A path manifest is created to map optimized names back to original paths
--
-- === Optimized Naming
--
-- Files are renamed for Claude's UI by:
--
-- * Replacing directory separators with dashes
-- * Flattening the directory structure
-- * Special handling for hidden files (e.g., .gitignore becomes dot--gitignore)
-- * Special handling for certain file types (e.g., .svg files become .xml)
--
-- This ensures that all files can be easily distinguished in Claude's UI
-- while maintaining a mapping back to their original locations. Hidden files
-- are transformed to be visible in file browsers while preserving their hidden
-- status when written back to the filesystem.

module Clod.FileSystem
  ( -- * Re-exports from FileSystem.Operations
    findAllFiles
  , safeRemoveFile
  , copyFile
  , safeReadFile
  , safeWriteFile
  , safeCopyFile
    
    -- * Re-exports from FileSystem.Detection
  , isModifiedSince
  , isTextFile
  , isTextDescription
  , needsTransformation
  , safeFileExists
  , safeIsTextFile
  
    -- * Re-exports from FileSystem.Processing
  , processFiles
  , ManifestEntry(..)
  , createOptimizedName
  , writeManifestFile
  
    -- * Re-exports from FileSystem.Transformations
  , transformFilename
  , flattenPath
  , sanitizeFilename
  , transformFileContent
  ) where

-- Re-export from FileSystem.Operations
import Clod.FileSystem.Operations (findAllFiles, safeRemoveFile, copyFile, 
                                 safeReadFile, safeWriteFile, safeCopyFile)

-- Re-export from FileSystem.Detection
import Clod.FileSystem.Detection (isModifiedSince, isTextFile, isTextDescription,
                                needsTransformation, safeFileExists, safeIsTextFile)

-- Re-export from FileSystem.Processing
import Clod.FileSystem.Processing (processFiles, ManifestEntry(..), 
                                  createOptimizedName, writeManifestFile)

-- Re-export from FileSystem.Transformations
import Clod.FileSystem.Transformations (transformFilename, flattenPath, 
                                      sanitizeFilename, transformFileContent)