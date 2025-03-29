{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Clod.FileSystem.Transformations
-- Description : File type transformations for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module handles special file type transformations required for Claude AI compatibility.
-- It contains transformations like SVG to XML conversion that are required due to 
-- external constraints from Claude's Project Knowledge system, as well as path flattening
-- and sanitization functions for improved compatibility.
--
-- Note: These transformations exist solely due to limitations in Claude's file format support
-- and may be removed in future versions if those limitations are lifted.

module Clod.FileSystem.Transformations
  ( transformFilename
  , flattenPath
  , sanitizeFilename
  , transformFileContent
  ) where

import qualified Data.List as L
import System.FilePath (takeExtension, takeFileName)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Polysemy
import Polysemy.Error

import Clod.Types
import Clod.Capability
import Clod.Effects

-- | Transform filename for Claude compatibility
--
-- This function applies special transformations to filenames to ensure 
-- they can be properly uploaded to Claude's Project Knowledge system.
--
-- Currently handles:
-- * SVG files - converted to XML extension for Claude compatibility
-- * Path flattening and sanitization for improved compatibility
--
-- @
-- transformFilename "logo.svg" -- returns "logo-svg.xml"
-- transformFilename "image.png" -- returns "image.png" (no change)
-- @
transformFilename :: String -> String -> String
transformFilename name original
  -- SVG files must be transformed to XML for Claude compatibility
  | ".svg" `L.isSuffixOf` original = 
      -- For SVG files, convert to XML extension
      let baseName = take (length name - 4) name
      in sanitizeFilename $ baseName ++ "-svg.xml"
  -- All other files remain unchanged but sanitized
  | otherwise = sanitizeFilename name

-- | Flatten a path by removing directory separators and replacing them
-- This makes paths suitable for flat storage
flattenPath :: FilePath -> FilePath
flattenPath path = 
  let filename = takeFileName path
      -- If there's more to the path than just the filename, we replace / and \ with _
      flattenedPath = if length path > length filename
                      then map replacePathSep path
                      else path
  in flattenedPath
  where
    replacePathSep '/' = '_'
    replacePathSep '\\' = '_'
    replacePathSep c = c

-- | Sanitize a filename by removing special characters
-- This ensures filenames are valid across platforms
sanitizeFilename :: FilePath -> FilePath
sanitizeFilename "" = "unnamed" -- Default name for empty strings
sanitizeFilename filename =
  let (name, ext) = splitExtension filename
      sanitizedName = filter isValidChar name
      sanitizedName' = if null sanitizedName then "unnamed" else sanitizedName
  in sanitizedName' ++ ext
  where
    isValidChar c = isAlphaNum c || c == '.' || c == '_' || c == '-'
    
    -- Split filename into name and extension
    splitExtension path =
      let ext = takeExtension path
          name = take (length path - length ext) path
      in (name, ext)

-- | Transform file content for Claude compatibility
-- This function is used for special file types that need transformation
transformFileContent :: Members '[FileSystem, Error ClodError, Embed IO] r
                     => FileReadCap -> FileWriteCap 
                     -> (BS.ByteString -> T.Text) -- ^ Transformation function
                     -> FilePath -> FilePath -> Sem r ()
transformFileContent readCap writeCap transformFn srcPath destPath = do
  -- Read with capability check
  content <- safeReadFile readCap srcPath
  
  -- Apply transformation
  let transformedContent = transformFn content
  
  -- Write with capability check
  safeWriteFile writeCap destPath (TE.encodeUtf8 transformedContent)
