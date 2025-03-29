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
import System.FilePath ((</>), takeExtension)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Char (toLower)
import Data.Bits ((.&.))
import System.IO.Unsafe (unsafePerformIO)

import Clod.Config (FileTypes(..), BinarySignature(..), BinarySignatures(..), 
                    loadFileTypes, loadBinarySignatures)
import Clod.Types (ClodM, FileReadCap(..), ClodError(..), isPathAllowed)

-- | Sample size for binary detection (in bytes)
sampleSize :: Int
sampleSize = 8192  -- Increased from 512 to get a better sample


-- | Check if a file is a text file (not binary)
--
-- This function uses a pure Haskell implementation to determine if a file is text or binary.
-- It examines the content of the file rather than relying on an external command.
--
-- The strategy is to:
-- 1. Check for file signatures (magic bytes) of common binary formats
-- 2. Check for null bytes (common in binary files)
-- 3. Validate UTF-8 encoding
-- 4. Calculate the ratio of control and non-printable characters
-- 5. Consider specific extensions as text or binary
--
-- This approach is more reliable and cross-platform than using external tools.
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  -- Check if file exists
  exists <- liftIO $ doesFileExist file
  if not exists
    then return False
    else do
      -- Read a larger sample from the beginning of the file
      result <- liftIO $ try $ BS.readFile file :: ClodM (Either IOError BS.ByteString)
      case result of
        Left _ -> return False  -- On error, assume it's not a text file
        Right content -> return $ isTextContent file content

-- | Check if content appears to be text
isTextContent :: FilePath -> BS.ByteString -> Bool
isTextContent file content = unsafePerformIO $ do
  -- Use unsafePerformIO to allow using FileTypes in a pure function
  -- This is acceptable because the function remains referentially transparent
  -- (same input will always produce the same output)
  fileTypes <- loadFileTypes
  signatures <- loadBinarySignatures
  return $ isTextContentPure file content fileTypes signatures

-- | Pure implementation of text content detection
isTextContentPure :: FilePath -> BS.ByteString -> FileTypes -> BinarySignatures -> Bool
isTextContentPure file content fileTypes binarySigs = 
  let 
      -- Take a sample from the beginning of the file
      sample = BS.take sampleSize content
      
      -- 1. Check for binary file signatures (magic bytes)
      hasSignature = any (\sig -> matchesSignature sample (bytes sig)) (signatures binarySigs)
      
      -- 2. Check for null bytes
      hasNullByte = BS.elem 0 sample
      
      -- 3. Check validity of UTF-8 encoding
      isValidUtf8 = validateUtf8 sample
      
      -- 4. Analyze control and non-printable characters
      controlCharCount = BS.length $ BS.filter isControlNonPrintChar sample
      totalBytes = BS.length sample
      -- Nothing to analyze in an empty file
      isEmpty = totalBytes == 0
      -- Calculate ratio of control chars (avoid division by zero)
      controlCharRatio = if isEmpty 
                           then 0.0 
                           else (fromIntegral controlCharCount :: Double) / 
                                (fromIntegral totalBytes :: Double)
      
      -- 5. Check extensions (using loaded configuration)
      extension = L.map toLower $ takeExtension file
      -- Check if it's a known text extension
      isTextExt = extension `elem` textExtensions fileTypes || 
                  any (`L.isSuffixOf` L.map toLower file) (textSpecialCases fileTypes)
      -- Check if it's a known binary extension
      isBinaryExt = extension `elem` binaryExtensions fileTypes ||
                    any (`L.isSuffixOf` L.map toLower file) (binarySpecialCases fileTypes)
      
      -- Combined checks: file is text if:
      -- - It's empty (benefit of doubt) OR
      -- - It has a known text extension AND no null bytes OR
      -- - It passes all the binary detection tests:
      --   - No binary signature
      --   - No null bytes (this is a hard requirement)
      --   - Valid UTF-8
      --   - Low ratio of control characters (<0.2)
      --   - Not a known binary extension
      isText = isEmpty || 
               (isTextExt && not hasNullByte) || 
               (not hasSignature && 
                not hasNullByte && 
                isValidUtf8 && 
                controlCharRatio < 0.2 && 
                not isBinaryExt)
  in isText

-- | Check if a byte is a control or non-printable character 
-- (excluding common whitespace: tabs, newlines, carriage returns)
isControlNonPrintChar :: Word8 -> Bool
isControlNonPrintChar b = 
  -- Control chars below 32, except tab (9), LF (10), CR (13)
  (b < 32 && b /= 9 && b /= 10 && b /= 13) || 
  -- DEL (127)
  b == 127 ||
  -- Check characters above ASCII range that aren't valid UTF-8 continuation bytes
  (b >= 128 && b < 192)

-- | Check if a byte sequence matches a binary file signature
matchesSignature :: BS.ByteString -> [Word8] -> Bool
matchesSignature bs sig
  | BS.length bs < length sig = False
  | otherwise = BS.take (length sig) bs == BS.pack sig

-- | Simple UTF-8 validation
-- Checks that multi-byte sequences are properly formed
validateUtf8 :: BS.ByteString -> Bool
validateUtf8 bytes = validateBytes (BS.unpack bytes) 0
  where
    validateBytes :: [Word8] -> Int -> Bool
    validateBytes [] _ = True
    validateBytes (b:rest) 0
      | b < 128 = validateBytes rest 0                  -- ASCII char
      | b .&. 0xE0 == 0xC0 = validateBytes rest 1       -- 2-byte sequence
      | b .&. 0xF0 == 0xE0 = validateBytes rest 2       -- 3-byte sequence
      | b .&. 0xF8 == 0xF0 = validateBytes rest 3       -- 4-byte sequence
      | otherwise = False                             -- Invalid leading byte
    validateBytes (b:rest) n
      | b .&. 0xC0 == 0x80 = validateBytes rest (n-1)   -- Valid continuation byte
      | otherwise = False                             -- Invalid continuation byte


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