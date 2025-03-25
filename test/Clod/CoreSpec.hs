{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.CoreSpec
-- Description : Tests for Core module functionality
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module contains tests for core Clod functionality.

module Clod.CoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (unless)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)

import Clod.Core
import Clod.Types

-- | Test specification for Core module
spec :: Spec
spec = do
  describe "initializeConfig" $ do
    it "creates a valid configuration with correct paths" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Initialize a git repository
        createDirectoryIfMissing True (tmpDir </> ".git")
        
        -- Create timestamp
        now <- getCurrentTime
        let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
        
        -- Call initializeConfig via ClodM monad
        configEither <- runClodM $ initializeConfig tmpDir (tmpDir </> "staging") True
        case configEither of
          Left err -> expectationFailure $ "Failed to initialize config: " ++ show err
          Right config -> do
            projectPath config `shouldBe` tmpDir
            stagingDir config `shouldBe` tmpDir </> "staging"
            configDir config `shouldBe` tmpDir </> ".claude-uploader"
            lastRunFile config `shouldBe` tmpDir </> ".claude-uploader" </> "last-run-marker"
            testMode config `shouldBe` True
            
            -- Check if directories were created
            doesDirectoryExist (configDir config) >>= \exists ->
              exists `shouldBe` True
              
            doesDirectoryExist (stagingDir config) >>= \exists ->
              exists `shouldBe` True
              
            doesDirectoryExist (currentStaging config) >>= \exists ->
              exists `shouldBe` True
  
  describe "createStagingDir" $ do
    it "creates a staging directory with timestamp" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create config
        let config = ClodConfig {
              projectPath = tmpDir,
              stagingDir = tmpDir </> "staging",
              configDir = tmpDir </> ".claude-uploader", 
              lastRunFile = tmpDir </> ".claude-uploader" </> "last-run-marker",
              timestamp = "20250325",
              currentStaging = "",
              testMode = True,
              ignorePatterns = []
            }
        
        -- Create staging directory
        createDirectoryIfMissing True (stagingDir config)
        
        -- Call createStagingDir via ClodM monad
        stagingDirEither <- runClodM $ createStagingDir config
        case stagingDirEither of
          Left err -> expectationFailure $ "Failed to create staging dir: " ++ show err
          Right dirPath -> do
            doesDirectoryExist dirPath >>= \exists ->
              exists `shouldBe` True
              
            -- Staging dir should start with ClaudeUpload_
            takeFileName dirPath `shouldSatisfy` ("ClaudeUpload_" `isPrefixOf`)
  
  describe "prepareManifest" $ do
    it "creates a valid manifest file" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        let manifestPath = tmpDir </> "manifest.json"
        
        -- Call prepareManifest via ClodM monad
        resultEither <- runClodM $ prepareManifest manifestPath
        case resultEither of
          Left err -> expectationFailure $ "Failed to prepare manifest: " ++ show err
          Right _ -> do
            -- Check if manifest file was created
            doesFileExist manifestPath >>= \exists ->
              exists `shouldBe` True
              
            -- Check if manifest file starts with "{"
            content <- readFile manifestPath
            content `shouldBe` "{\n"