{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.Detection
-- Description : File detection operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for detecting file types and states,
-- including determining if a file is a text file or has been modified.

module Clod.FileSystem.Detection
  ( -- * File type detection
    isTextFile
  , isModifiedSince
  ) where

import Control.Exception (try)
import qualified Data.List as L
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((</>))
import System.Process (readProcess)

import Clod.Types (ClodM, liftIO)

-- | Check if a file is a text file (not binary)
--
-- This function uses the 'file' command to determine if a file is a text file.
-- It doesn't rely on extensions because that approach is error-prone and requires
-- maintaining lists of extensions, which should be in configuration files.
--
-- The strategy is to use the system's 'file' command to detect the MIME type,
-- and consider anything that starts with "text/" as a text file.
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  -- Use 'file' command to check mime type
  result <- liftIO $ try (readProcess "file" ["--mime-type", "-b", file] "") :: ClodM (Either IOError String)
  case result of
    Left _ -> return False  -- On error, assume it's not a text file
    Right mimeType -> return $ "text/" `L.isPrefixOf` mimeType || 
                              "application/json" `L.isPrefixOf` mimeType ||
                              "application/xml" `L.isPrefixOf` mimeType

-- | Check if a file has been modified since the given time
isModifiedSince :: FilePath -> UTCTime -> FilePath -> ClodM Bool
isModifiedSince basePath lastRunTime relPath = do
  let fullPath = basePath </> relPath
  fileExists <- liftIO $ doesFileExist fullPath
  if not fileExists
    then return False
    else do
      modTime <- liftIO $ getModificationTime fullPath
      return (modTime > lastRunTime)