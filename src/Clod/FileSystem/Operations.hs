{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.Operations
-- Description : File system operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides basic file system operations like finding files,
-- copying files, and safely removing files.

module Clod.FileSystem.Operations
  ( -- * File operations
    findAllFiles
  , copyFile
  , safeRemoveFile
  ) where

import Control.Monad (when)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile)
import System.FilePath ((</>))
import qualified System.Directory as D (copyFile)

import Clod.Types (ClodM, liftIO)

-- | Recursively find all files in a directory
--
-- This function takes a base path and a list of files/directories,
-- and recursively finds all files within those directories.
-- It returns paths relative to the base path.
--
-- @
-- -- Find all files in the "src" directory
-- files <- findAllFiles "/path/to/repo" ["src"]
--
-- -- Find all files in multiple directories
-- files <- findAllFiles "/path/to/repo" ["src", "docs", "tests"]
-- @
findAllFiles :: FilePath -> [FilePath] -> ClodM [FilePath]
findAllFiles basePath = fmap concat . mapM findFilesRecursive
  where
    findFilesRecursive :: FilePath -> ClodM [FilePath]
    findFilesRecursive file = do
      let fullPath = basePath </> file
      isDir <- liftIO $ doesDirectoryExist fullPath
      
      case isDir of
        False -> return [file]  -- Just return the file path
        True  -> do
          -- Get directory contents, excluding "." and ".."
          contents <- liftIO $ getDirectoryContents fullPath
          let validContents = filter (`notElem` [".", ".."]) contents
          
          -- Recursively process subdirectories
          subFiles <- findAllFiles fullPath validContents
          
          -- Prepend current path to subdirectory files
          return $ map (file </>) subFiles

-- | Copy a file from source to destination
--
-- This is a simple wrapper around System.Directory.copyFile
-- that makes it easier to use in the ClodM monad.
copyFile :: FilePath -> FilePath -> ClodM ()
copyFile source dest = liftIO $ D.copyFile source dest

-- | Safely remove a file, ignoring errors if it doesn't exist
safeRemoveFile :: FilePath -> ClodM ()
safeRemoveFile path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path