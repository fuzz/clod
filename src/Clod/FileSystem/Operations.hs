{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.Operations
-- Description : File system operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module provides basic file system operations like finding files,
-- copying files, and safely removing files.

module Clod.FileSystem.Operations
  ( -- * File operations
    findAllFiles
  , copyFile
  , safeRemoveFile
  , safeReadFile
  , safeWriteFile
  , safeCopyFile
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, removeFile, 
                        copyFile, canonicalizePath)
import System.FilePath ((</>))
import qualified Data.ByteString as BS

import Clod.Types (ClodM, FileReadCap(..), FileWriteCap(..), ClodError(..), isPathAllowed,
                   allowedReadDirs, allowedWriteDirs, (^.))

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
--
-- -- Find all files in the root directory (without "./" prefix)
-- files <- findAllFiles "/path/to/repo" [""]
-- @
findAllFiles :: FilePath -> [FilePath] -> ClodM [FilePath]
findAllFiles basePath = fmap concat . mapM findFilesRecursive
  where
    findFilesRecursive :: FilePath -> ClodM [FilePath]
    findFilesRecursive file = do
      -- Special case for empty string or "." to handle root directory
      -- without adding a "./" prefix to paths
      let useBasePath = null file || file == "."
          fullPath = if useBasePath then basePath else basePath </> file
      
      isDir <- liftIO $ doesDirectoryExist fullPath
      
      case isDir of
        False -> return [file]  -- Just return the file path
        True  -> do
          -- Get directory contents, excluding "." and ".."
          contents <- liftIO $ getDirectoryContents fullPath
          let validContents = filter (`notElem` [".", ".."]) contents
          
          -- Recursively process subdirectories
          subFiles <- findAllFiles fullPath validContents
          
          -- Prepend current path to subdirectory files, but only if not the root dir
          return $ if useBasePath
                  then subFiles  -- For root dir, don't add any prefix
                  else map (file </>) subFiles  -- Otherwise add directory prefix

-- | Safely remove a file, ignoring errors if it doesn't exist
safeRemoveFile :: FilePath -> ClodM ()
safeRemoveFile path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

-- | Safe file reading that checks capabilities
safeReadFile :: FileReadCap -> FilePath -> ClodM BS.ByteString
safeReadFile cap path = do
  allowed <- liftIO $ isPathAllowed (cap ^. allowedReadDirs) path
  if allowed
    then liftIO $ BS.readFile path
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ "Access denied: Cannot read file outside allowed directories: " ++ canonicalPath

-- | Safe file writing that checks capabilities
safeWriteFile :: FileWriteCap -> FilePath -> BS.ByteString -> ClodM ()
safeWriteFile cap path content = do
  allowed <- liftIO $ isPathAllowed (cap ^. allowedWriteDirs) path
  if allowed
    then liftIO $ BS.writeFile path content
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ "Access denied: Cannot write file outside allowed directories: " ++ canonicalPath

-- | Safe file copying that checks capabilities for both read and write
safeCopyFile :: FileReadCap -> FileWriteCap -> FilePath -> FilePath -> ClodM ()
safeCopyFile readCap writeCap src dest = do
  srcAllowed <- liftIO $ isPathAllowed (readCap ^. allowedReadDirs) src
  destAllowed <- liftIO $ isPathAllowed (writeCap ^. allowedWriteDirs) dest
  if srcAllowed && destAllowed
    then liftIO $ copyFile src dest
    else do
      canonicalSrc <- liftIO $ canonicalizePath src
      canonicalDest <- liftIO $ canonicalizePath dest
      let errorMsg = if not srcAllowed && not destAllowed
                     then "Access denied: Both source and destination paths violate restrictions"
                     else if not srcAllowed
                          then "Access denied: Source path violates restrictions: " ++ canonicalSrc
                          else "Access denied: Destination path violates restrictions: " ++ canonicalDest
      throwError $ CapabilityError errorMsg