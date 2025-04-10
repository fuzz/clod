{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.TestHelpers
-- Description : Helper functions for testing Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module provides helper functions for testing Clod functionality.

module Clod.TestHelpers
  ( defaultTestConfig
  ) where

import System.FilePath ((</>))
import Clod.Types (ClodConfig(..), 
                  projectPath, stagingDir, configDir, databaseFile, previousStaging,
                  flushMode, lastMode, timestamp, currentStaging, testMode, 
                  verbose, ignorePatterns, (&), (.~))

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
-- let customConfig = defaultTestConfig tmpDir & verbose .~ True & flushMode .~ True
-- @
defaultTestConfig :: FilePath -> ClodConfig
defaultTestConfig tmpDir = 
  -- Start with empty config
  ClodConfig "" "" "" "" "" "" Nothing False False False False []
  -- Apply lenses to set values
  & projectPath .~ tmpDir
  & stagingDir .~ tmpDir </> "staging"
  & configDir .~ tmpDir </> ".clod"
  & databaseFile .~ tmpDir </> ".clod" </> "db.dhall"
  & previousStaging .~ Nothing
  & flushMode .~ False
  & lastMode .~ False
  & timestamp .~ "20250401-000000"
  & currentStaging .~ tmpDir </> "staging"
  & testMode .~ True
  & verbose .~ False
  & ignorePatterns .~ []