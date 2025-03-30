{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clod.FileSystem.Detection
-- Description : File detection operations for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for detecting file types and states
-- using libmagic for robust file type detection. It determines whether
-- files are text or binary based on their content rather than just extensions.

module Clod.FileSystem.Detection
  ( -- * File type detection
    isTextFile
  , isModifiedSince
  , safeFileExists
  , safeIsTextFile
  , isMimeTypeText
  , needsTransformation
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isInfixOf)
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime, canonicalizePath)
import System.FilePath ((</>), takeFileName, takeExtension)

import Clod.Types (ClodM, FileReadCap(..), ClodError(..), isPathAllowed)
import qualified Magic.Init as Magic
import qualified Magic.Operations as Magic
import qualified Magic.Types as Magic

-- | Check if a file is a text file using libmagic
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  exists <- liftIO $ doesFileExist file
  if not exists
    then return False
    else do
      result <- liftIO $ try $ do
        magic <- Magic.magicOpen [Magic.MagicMimeType]
        Magic.magicLoadDefault magic
        mime <- Magic.magicFile magic file
        return $ isMimeTypeText mime
      case result of
        Left (_ :: SomeException) -> return False
        Right isText -> return isText

-- | Helper to determine if a MIME type represents text content
isMimeTypeText :: String -> Bool
isMimeTypeText mime =
  "text/" `isPrefixOf` mime ||
  mime == "application/json" ||
  mime == "application/xml" ||
  mime == "application/javascript" ||
  mime == "application/x-shell" ||
  mime == "application/x-shellscript" ||
  "script" `isInfixOf` mime

-- | Special handling for files that need transformation
needsTransformation :: FilePath -> Bool
needsTransformation path =
  -- Handle dotfiles and SVG files with special transformation
  ("." `isPrefixOf` takeFileName path) || 
  (takeExtension path == ".svg")

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

-- | Safe file existence check that checks capabilities
safeFileExists :: FileReadCap -> FilePath -> ClodM Bool
safeFileExists cap path = do
  allowed <- liftIO $ isPathAllowed (allowedReadDirs cap) path
  if allowed
    then liftIO $ doesFileExist path
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ "Access denied: Cannot check existence of file outside allowed directories: " ++ canonicalPath

-- | Safe file type check that checks capabilities
safeIsTextFile :: FileReadCap -> FilePath -> ClodM Bool
safeIsTextFile cap path = do
  allowed <- liftIO $ isPathAllowed (allowedReadDirs cap) path
  if allowed
    then isTextFile path
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ "Access denied: Cannot check file type outside allowed directories: " ++ canonicalPath