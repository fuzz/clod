{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.ConfigSpec
-- Description : Tests for configuration handling
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for the configuration loading and handling functionality.

module Clod.ConfigSpec (spec) where

import Test.Hspec
import System.Environment (setEnv, unsetEnv)
import Control.Exception (bracket)

import Clod.Config

-- | Helper to set and unset environment variable
withEnv :: String -> String -> IO a -> IO a
withEnv name value action = bracket 
  (setEnv name value >> return ())
  (\_ -> unsetEnv name)
  (\_ -> action)

-- | Test specification for Config module
spec :: Spec
spec = do
  configDirNameSpec
  clodIgnoreFileSpec
  clodConfigDirSpec

-- | Tests for configDirName function
configDirNameSpec :: Spec
configDirNameSpec = describe "configDirName" $ do
  it "returns default config directory name when environment variable is not set" $ do
    -- Reset environment variable if it exists
    unsetEnv "CLOD_DIR"
    
    -- Get default directory name
    dirName <- configDirName
    
    -- Verify default value
    dirName `shouldBe` ".clod"
    
  it "returns environment variable value when set" $ do
    -- Test with environment variable
    withEnv "CLOD_DIR" "test-config-dir" $ do
      dirName <- configDirName
      dirName `shouldBe` "test-config-dir"
      
  it "returns default when environment variable is empty" $ do
    -- Test with empty environment variable
    withEnv "CLOD_DIR" "" $ do
      dirName <- configDirName
      dirName `shouldBe` ".clod"

-- | Tests for clodIgnoreFile function
clodIgnoreFileSpec :: Spec
clodIgnoreFileSpec = describe "clodIgnoreFile" $ do
  it "returns default ignore file name when environment variable is not set" $ do
    -- Reset environment variable if it exists
    unsetEnv "CLODIGNORE"
    
    -- Get default ignore file name
    fileName <- clodIgnoreFile
    
    -- Verify default value
    fileName `shouldBe` ".clodignore"
    
  it "returns environment variable value when set" $ do
    -- Test with environment variable
    withEnv "CLODIGNORE" "test-ignore-file" $ do
      fileName <- clodIgnoreFile
      fileName `shouldBe` "test-ignore-file"
      
  it "returns default when environment variable is empty" $ do
    -- Test with empty environment variable
    withEnv "CLODIGNORE" "" $ do
      fileName <- clodIgnoreFile
      fileName `shouldBe` ".clodignore"

-- | Tests for clodConfigDir function
clodConfigDirSpec :: Spec
clodConfigDirSpec = describe "clodConfigDir" $ do
  it "builds correct config directory path with default name" $ do
    -- Reset environment variable
    unsetEnv "CLOD_DIR"
    
    -- Test with a sample path
    let rootPath = "/test/project/path"
    
    -- Get config directory path
    configDir <- clodConfigDir rootPath
    
    -- Verify path
    configDir `shouldBe` "/test/project/path/.clod"
    
  it "builds correct config directory path with environment variable" $ do
    -- Test with environment variable
    withEnv "CLOD_DIR" "custom-config" $ do
      let rootPath = "/test/project/path"
      
      configDir <- clodConfigDir rootPath
      
      configDir `shouldBe` "/test/project/path/custom-config"
      
  it "works with relative paths" $ do
    -- Test with a relative path
    configDir <- clodConfigDir "project"
    
    -- Should append config dir to the path
    configDir `shouldBe` "project/.clod"
    
