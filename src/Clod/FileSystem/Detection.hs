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
  , isTextContent
  , isModifiedSince
  , safeFileExists
  , safeIsTextFile
  ) where

import Control.Exception (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime, canonicalizePath)
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import Data.Char (toLower)

import Clod.Types (ClodM, FileReadCap(..), ClodError(..), isPathAllowed)

-- | Check if a file is a text file (not binary)
--
-- This function uses a pure Haskell implementation to determine if a file is text or binary.
-- It examines the content of the file rather than relying on an external command.
--
-- The strategy is to:
-- 1. Check for null bytes (common in binary files)
-- 2. Calculate the ratio of control characters (excluding whitespace)
-- 3. Consider specific MIME types (JSON, XML) as text
--
-- This approach is more reliable and cross-platform than using external tools.
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  -- Check if file exists
  exists <- liftIO $ doesFileExist file
  if not exists
    then return False
    else do
      -- Read up to 512 bytes from the beginning of the file
      result <- liftIO $ try $ BS.readFile file :: ClodM (Either IOError BS.ByteString)
      case result of
        Left _ -> return False  -- On error, assume it's not a text file
        Right content -> return $ isTextContent file content

-- | Check if content appears to be text
isTextContent :: FilePath -> BS.ByteString -> Bool
isTextContent file content = 
  let sample = BS.take 512 content
      -- Check for common text file characteristics
      hasNullByte = BS.elem 0 sample
      controlCharCount = BS.length (BS.filter isControlChar sample)
      controlCharRatio = (fromIntegral controlCharCount :: Double) / 
                       max 1.0 ((fromIntegral (BS.length sample)) :: Double)
      -- Check file extension for specific types we know are text
      isJson = ".json" `L.isSuffixOf` L.map toLower file
      isXml = ".xml" `L.isSuffixOf` L.map toLower file || ".svg" `L.isSuffixOf` L.map toLower file
      -- Text files shouldn't have many control characters and definitely no NULL bytes
      isText = (not hasNullByte && controlCharRatio < 0.3) || isJson || isXml
  in isText
  where
    -- Check if a byte is a control character (excluding tabs, newlines and carriage returns)
    isControlChar b = b < 32 && b /= 9 && b /= 10 && b /= 13

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