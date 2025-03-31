{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.TestHelpers
-- Description : Helper functions for testing Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides helper functions for testing Clod functionality.

module Clod.TestHelpers
  ( defaultTestConfig
  ) where

import System.FilePath ((</>))
import Clod.Types (ClodConfig(..))

-- | Default config for testing
-- 
-- Creates a standard configuration object for use in tests with reasonable defaults
-- that can be easily overridden when needed.
--
-- @
-- -- Use with default values
-- let config = defaultTestConfig tmpDir
-- 
-- -- Override specific values
-- let customConfig = (defaultTestConfig tmpDir) { verbose = True, flushMode = True }
-- @
defaultTestConfig :: FilePath -> ClodConfig
defaultTestConfig tmpDir = ClodConfig
  { projectPath = tmpDir
  , stagingDir = tmpDir </> "staging"
  , configDir = tmpDir </> ".clod"
  , databaseFile = tmpDir </> ".clod" </> "db.dhall"
  , previousStaging = Nothing
  , flushMode = False
  , lastMode = False
  , timestamp = "20250401-000000"
  , currentStaging = tmpDir </> "staging"
  , testMode = True
  , verbose = False
  , ignorePatterns = []
  }