{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Config
-- Description : Configuration handling for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for handling configuration options
-- including environment variables and default values.

module Clod.Config
  ( -- * Configuration functions
    configDirName
  , clodIgnoreFile
  , clodConfigDir
  ) where

import System.Environment (lookupEnv)
import System.FilePath ((</>))

-- | Get configuration directory name
--
-- Returns the configuration directory name, checking the CLOD_DIR
-- environment variable first and falling back to ".clod" if not set.
--
-- @
-- configDir <- configDirName  -- Returns ".clod" or value of CLOD_DIR
-- @
configDirName :: IO String
configDirName = do
  envValue <- lookupEnv "CLOD_DIR"
  return $ case envValue of
    Just value | not (null value) -> value
    _                            -> ".clod"

-- | Get clodignore file name
--
-- Returns the clodignore file name, checking the CLODIGNORE
-- environment variable first and falling back to ".clodignore" if not set.
--
-- @
-- ignoreFile <- clodIgnoreFile  -- Returns ".clodignore" or value of CLODIGNORE
-- @
clodIgnoreFile :: IO String
clodIgnoreFile = do
  envValue <- lookupEnv "CLODIGNORE"
  return $ case envValue of
    Just value | not (null value) -> value
    _                            -> ".clodignore"

-- | Build the config directory path from project root
--
-- @
-- configDir <- clodConfigDir "/path/to/project"  -- Returns "/path/to/project/.clod" or environment override
-- @
clodConfigDir :: FilePath -> IO FilePath
clodConfigDir rootPath = do
  dirName <- configDirName
  return $ rootPath </> dirName