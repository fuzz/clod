{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.CoreSpec
-- Description : Tests for Core module functionality
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for core Clod functionality.

module Clod.CoreSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory, getCanonicalTemporaryDirectory)
import Data.List (isPrefixOf)

import Clod.Core
import Clod.Types

-- | Test specification for Core module
spec :: Spec
spec = do
  describe "initializeConfig" $ do
    it "creates a valid configuration with temporary directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Initialize a git repository
        createDirectoryIfMissing True (tmpDir </> ".git")
        
        -- Call initializeConfig via ClodM monad
        configEither <- runClodM $ initializeConfig tmpDir "" True
        case configEither of
          Left err -> expectationFailure $ "Failed to initialize config: " ++ show err
          Right config -> do
            projectPath config `shouldBe` tmpDir
            configDir config `shouldBe` tmpDir </> ".clod"
            lastRunFile config `shouldBe` tmpDir </> ".clod" </> "last-run-marker"
            testMode config `shouldBe` True
            
            -- Check if config directory was created
            doesDirectoryExist (configDir config) >>= \exists ->
              exists `shouldBe` True
              
            -- Check if staging directory was created and is a temp directory
            doesDirectoryExist (stagingDir config) >>= \exists ->
              exists `shouldBe` True
              
            -- In test mode with empty staging dir arg, should still create a temp directory
            tmpRoot <- getCanonicalTemporaryDirectory
            stagingDir config `shouldSatisfy` (\path -> tmpRoot `isPrefixOf` path)
            
            -- Should have the correct prefix format
            takeFileName (stagingDir config) `shouldSatisfy` ("clod_" `isPrefixOf`)
  
  describe "createTempStagingDir" $ do
    it "creates a temp staging directory with proper naming" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a minimal config for testing
        let config = ClodConfig {
              projectPath = tmpDir,
              stagingDir = "",
              configDir = tmpDir </> ".clod", 
              lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
              timestamp = "20250325",
              currentStaging = "",
              testMode = True,
              ignorePatterns = []
            }
        
        -- Create the config directory
        createDirectoryIfMissing True (configDir config)
        
        -- Call createTempStagingDir via ClodM monad
        result <- runClodM $ createTempStagingDir config
        case result of
          Left err -> expectationFailure $ "Failed to create staging dir: " ++ show err
          Right stagingPath -> do
            -- Verify directory exists
            doesDirectoryExist stagingPath >>= \exists ->
              exists `shouldBe` True
              
            -- Verify it's in the system temp directory
            tmpRoot <- getCanonicalTemporaryDirectory
            stagingPath `shouldSatisfy` (\path -> tmpRoot `isPrefixOf` path)
            
            -- Verify the naming convention
            takeFileName stagingPath `shouldSatisfy` ("clod_20250325_" `isPrefixOf`)
            
            -- Verify tracking file was created
            trackingFile <- doesFileExist (tmpDir </> ".clod" </> "last-temp-dir")
            trackingFile `shouldBe` True
  
  describe "cleanupPreviousTempDir" $ do
    it "cleans up previous temporary directory if it exists" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a minimal config for testing
        let config = ClodConfig {
              projectPath = tmpDir,
              stagingDir = "",
              configDir = tmpDir </> ".clod", 
              lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
              timestamp = "20250325",
              currentStaging = "",
              testMode = True,
              ignorePatterns = []
            }
        
        -- Create the config directory
        createDirectoryIfMissing True (configDir config)
        
        -- Create a previous temp directory to clean up
        prevTempDir <- getCanonicalTemporaryDirectory
        let prevDirPath = prevTempDir </> "clod_previous_test"
        createDirectoryIfMissing True prevDirPath
        writeFile (prevDirPath </> "test-file.txt") "test content"
        
        -- Write the tracking file
        createDirectoryIfMissing True (tmpDir </> ".clod")
        writeFile (tmpDir </> ".clod" </> "last-temp-dir") prevDirPath
        
        -- Verify the directory exists before cleanup
        beforeExists <- doesDirectoryExist prevDirPath
        beforeExists `shouldBe` True
        
        -- Call cleanupPreviousTempDir via ClodM monad
        result <- runClodM $ cleanupPreviousTempDir config
        case result of
          Left err -> expectationFailure $ "Failed to clean up previous temp dir: " ++ show err
          Right _ -> do
            -- Verify the directory was removed
            afterExists <- doesDirectoryExist prevDirPath
            afterExists `shouldBe` False
