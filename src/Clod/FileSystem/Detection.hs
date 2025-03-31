{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , isTextDescription
  , needsTransformation
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (isPrefixOf)
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime, canonicalizePath)
import System.FilePath ((</>), takeFileName, takeExtension)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Dhall
import Data.FileEmbed (embedStringFile)

import Clod.Types (ClodM, FileReadCap(..), ClodError(..), isPathAllowed, ClodConfig(..))
import qualified Magic.Init as Magic
import qualified Magic.Operations as Magic

-- | Type to represent text patterns from Dhall
newtype TextPatterns = TextPatterns
  { textPatterns :: [T.Text]
  } deriving (Show, Eq)

instance Dhall.FromDhall TextPatterns where
  autoWith _ = Dhall.record $
    TextPatterns <$> Dhall.field "textPatterns" Dhall.auto

-- | Default text patterns content embedded at compile time
defaultTextPatternsContent :: String
defaultTextPatternsContent = BS.unpack $(embedStringFile "resources/text_patterns.dhall")

-- | Parse the embedded text patterns content
parseDefaultTextPatterns :: ClodM TextPatterns
parseDefaultTextPatterns = do
  result <- liftIO $ try $ Dhall.input Dhall.auto (T.pack defaultTextPatternsContent)
  case result of
    Right patterns -> return patterns
    Left (e :: SomeException) -> 
      throwError $ ConfigError $ "Failed to parse default text patterns: " ++ show e

-- | Load text patterns for determining text files
-- This first checks for a custom pattern file in the clod directory,
-- and falls back to the embedded default patterns if not found.
loadTextPatterns :: ClodM TextPatterns
loadTextPatterns = do
  clodDir <- asks configDir
  
  -- First check if there's a custom pattern file in the clod directory
  let customPath = clodDir </> "resources" </> "text_patterns.dhall"
  customExists <- liftIO $ doesFileExist customPath
  
  if customExists
    then do
      result <- liftIO $ try $ Dhall.inputFile Dhall.auto customPath
      case result of
        Right patterns -> return patterns
        Left (e :: SomeException) -> 
          throwError $ ConfigError $ "Failed to load custom text patterns: " ++ show e
    else parseDefaultTextPatterns

-- | Check if a file is a text file using libmagic with enhanced detection
isTextFile :: FilePath -> ClodM Bool
isTextFile file = do
  exists <- liftIO $ doesFileExist file
  if not exists
    then return False
    else do
      -- Detect the file type using libmagic
      result <- liftIO $ try $ do
        magic <- Magic.magicOpen []
        Magic.magicLoadDefault magic
        Magic.magicFile magic file
      
      case result of
        Left (_ :: SomeException) -> return False
        Right description -> isTextDescription description

-- | Helper to determine if a file description indicates text content
isTextDescription :: String -> ClodM Bool
isTextDescription desc = do
  patterns <- loadTextPatterns
  let lowerDesc = T.toLower $ T.pack desc
  return $ any (\pattern -> pattern `T.isInfixOf` lowerDesc) (textPatterns patterns)

-- | Special handling for files that need transformation
--
-- Detects files that need special handling and transformation based on their
-- name or extension. Currently identifies:
--
-- * Hidden files (dotfiles) - need to be transformed to be visible
-- * SVG files - need to be transformed to XML for Claude compatibility
--
-- >>> needsTransformation ".gitignore"
-- True
--
-- >>> needsTransformation "logo.svg"
-- True
--
-- >>> needsTransformation "regular-file.txt"
-- False
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