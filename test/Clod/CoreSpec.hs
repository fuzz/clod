{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
import System.IO.Temp (withSystemTempDirectory)
import Data.Either (isRight)
import qualified System.IO

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import Clod.Core
import Clod.Types (ClodConfig(..), ClodError(..), FileResult(..))
import Clod.Effects
import Clod.Capability

-- | Test specification for Core module
spec :: Spec
spec = do
  effectsApiSpec
  runClodAppSpec
  
-- | Tests for the effects-based API
effectsApiSpec :: Spec
effectsApiSpec = describe "Effects-based API" $ do
  it "can process text files with capabilities" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test files
      createDirectoryIfMissing True (tmpDir </> "src")
      System.IO.writeFile (tmpDir </> "src" </> "test.txt") "test content"
      
      -- Create a mock binary file (a small executable)
      createDirectoryIfMissing True (tmpDir </> "bin")
      System.IO.writeFile (tmpDir </> "bin" </> "testbin") "\x7F\x45\x4C\x46" -- ELF header magic
      
      -- Create test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            ignorePatterns = []
          }
      
      -- Create staging directory
      createDirectoryIfMissing True (stagingDir config)
      
      -- Create capabilities
      let readCap = fileReadCap [tmpDir]
          writeCap = fileWriteCap [tmpDir, stagingDir config]
      
      -- Run test with effects
      result <- runM . runError @ClodError . runReader config . runConsoleIO . runFileSystemIO $
        processFileWithEffects readCap writeCap (tmpDir </> "src" </> "test.txt") "src/test.txt"
      
      -- Verify result
      result `shouldSatisfy` isRight
      case result of
        Right Success -> do
          -- Check if file was copied to staging
          let destPath = tmpDir </> "staging" </> "test.txt"
          exists <- doesFileExist destPath
          exists `shouldBe` True
          
          -- Check content
          content <- System.IO.readFile destPath
          content `shouldBe` "test content"
        _ -> expectationFailure "File processing didn't succeed"
  
  it "respects capability restrictions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test files
      createDirectoryIfMissing True (tmpDir </> "src")
      System.IO.writeFile (tmpDir </> "src" </> "test.txt") "test content"
      createDirectoryIfMissing True (tmpDir </> "private")
      System.IO.writeFile (tmpDir </> "private" </> "secret.txt") "secret data"
      
      -- Create test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            ignorePatterns = []
          }
      
      -- Create staging directory
      createDirectoryIfMissing True (stagingDir config)
      
      -- Create limited capabilities (only src directory, not private)
      let readCap = fileReadCap [tmpDir </> "src"]
          writeCap = fileWriteCap [tmpDir </> "staging"]
      
      -- Try to access file outside allowed directory
      result <- runM . runError @ClodError . runReader config . runConsoleIO . runFileSystemIO $
        processFileWithEffects readCap writeCap (tmpDir </> "private" </> "secret.txt") "private/secret.txt"
      
      -- Should fail due to capability restriction
      result `shouldSatisfy` isLeft
      where
        isLeft (Left _) = True
        isLeft _ = False

-- | Tests for runClodApp
runClodAppSpec :: Spec
runClodAppSpec = describe "runClodApp" $ do
  it "initializes paths correctly" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a git repo structure
      createDirectoryIfMissing True (tmpDir </> ".git")
      
      -- Create a test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            ignorePatterns = []
          }
          
      -- Run initialization effect
      result <- runClodApp config "" False False True
      
      -- Should succeed
      result `shouldSatisfy` isRight
      
      -- Check if directories were created
      stagingExists <- doesDirectoryExist (tmpDir </> "staging")
      stagingExists `shouldBe` True
      
      configDirExists <- doesDirectoryExist (tmpDir </> ".clod")
      configDirExists `shouldBe` True
      
  it "updates last run marker" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a git repo structure
      createDirectoryIfMissing True (tmpDir </> ".git")
      createDirectoryIfMissing True (tmpDir </> ".clod")
      
      -- Create a test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            ignorePatterns = []
          }
          
      -- Run effects
      result <- runClodApp config "" False False True
      
      -- Should succeed
      result `shouldSatisfy` isRight
      
      -- Check if last run marker was created
      markerExists <- doesFileExist (tmpDir </> ".clod" </> "last-run-marker")
      markerExists `shouldBe` True
      
  it "properly honors capability restrictions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a git repo structure with a forbidden directory outside the repo
      createDirectoryIfMissing True (tmpDir </> ".git")
      let forbiddenDir = "/tmp/forbidden"  -- A directory outside our capability
      
      -- Create a test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            lastRunFile = tmpDir </> ".clod" </> "last-run-marker",
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            ignorePatterns = []
          }
      
      -- Test that our capability restricts access as expected
      result <- runM . runError @ClodError . runReader config . runFileSystemIO $ do
        -- Create capability that only allows access to the test directory
        let readCap = fileReadCap [tmpDir]
        
        -- Try to access a file outside our capability
        safeFileExists readCap forbiddenDir
      
      -- Should fail with a permission error
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> expectationFailure "Access was granted to a directory outside our capability"