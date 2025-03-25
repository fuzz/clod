{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystemSpec
-- Description : Tests for file system operations
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module contains tests for file system operations.

module Clod.FileSystemSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (unless, when)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.IORef (newIORef)
import Control.Monad.IO.Class (liftIO)

import Clod.FileSystem
import Clod.Types hiding (Success) -- Avoid name collision with QuickCheck.Success
import qualified Clod.Types as CT

-- | Test specification for file system operations
spec :: Spec
spec = do
  describe "findAllFiles" $ do
    it "correctly finds all files in a directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test directory structure
        createDirectoryIfMissing True (tmpDir </> "src" </> "components")
        createDirectoryIfMissing True (tmpDir </> "test")
        
        -- Create some test files
        writeFile (tmpDir </> "README.md") "# Test"
        writeFile (tmpDir </> "src" </> "index.js") "console.log('hello');"
        writeFile (tmpDir </> "src" </> "components" </> "Button.jsx") "<Button />"
        writeFile (tmpDir </> "test" </> "index.test.js") "test('example');"
        
        -- Test finding all files
        allFilesEither <- runClodM $ findAllFiles tmpDir ["README.md", "src", "test"]
        case allFilesEither of
          Left err -> expectationFailure $ "Failed to find files: " ++ show err
          Right files -> do
            files `shouldContain` ["README.md"]
            files `shouldContain` ["src/index.js"]
            files `shouldContain` ["src/components/Button.jsx"]
            files `shouldContain` ["test/index.test.js"]
            length files `shouldBe` 4
  
  describe "isTextFile" $ do
    it "identifies text files correctly" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test files of different types
        writeFile (tmpDir </> "text.txt") "plain text file"
        writeFile (tmpDir </> "script.js") "console.log('hello');"
        writeFile (tmpDir </> "style.css") "body { color: red; }"
        
        -- Get results via ClodM monad
        textResultEither <- runClodM $ isTextFile (tmpDir </> "text.txt")
        jsResultEither <- runClodM $ isTextFile (tmpDir </> "script.js")
        cssResultEither <- runClodM $ isTextFile (tmpDir </> "style.css")
        
        case (textResultEither, jsResultEither, cssResultEither) of
          (Right textResult, Right jsResult, Right cssResult) -> do
            textResult `shouldBe` True
            jsResult `shouldBe` True
            cssResult `shouldBe` True
          _ -> expectationFailure "Failed to identify text files"
  
  describe "isModifiedSince" $ do
    it "correctly identifies files modified after a timestamp" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Get current time
        now <- getCurrentTime
        let beforeTime = addUTCTime (-3600) now  -- 1 hour ago
        
        -- Create a file
        writeFile (tmpDir </> "file.txt") "test content"
        
        -- Check if file is modified since the timestamp
        resultEither <- runClodM $ isModifiedSince tmpDir beforeTime "file.txt"
        case resultEither of
          Left err -> expectationFailure $ "Failed to check modification time: " ++ show err
          Right result -> result `shouldBe` True
        
  describe "processFile" $ do
    it "correctly processes a text file" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create staging and manifest directories
        createDirectoryIfMissing True (tmpDir </> "staging")
        let manifestPath = tmpDir </> "manifest.json"
        writeFile manifestPath "{\n"
        
        -- Create config
        let config = ClodConfig {
              projectPath = tmpDir,
              stagingDir = tmpDir </> "staging",
              configDir = tmpDir </> ".claude-uploader", 
              lastRunFile = tmpDir </> ".claude-uploader" </> "last-run-marker",
              timestamp = "20250325",
              currentStaging = tmpDir </> "staging" </> "ClaudeUpload_20250325",
              testMode = True,
              ignorePatterns = []
            }
        
        -- Create staging directory
        createDirectoryIfMissing True (currentStaging config)
        
        -- Create a test file
        writeFile (tmpDir </> "test.js") "console.log('test');"
        
        -- Create a first entry ref
        firstEntryRef <- liftIO $ newIORef True
        
        -- Process the file
        resultEither <- runClodM $ processFile config manifestPath (tmpDir </> "test.js") "test.js" firstEntryRef
        case resultEither of
          Left err -> expectationFailure $ "Failed to process file: " ++ show err
          Right result -> do
            result `shouldBe` CT.Success
            doesFileExist (currentStaging config </> "test.js") >>= \exists ->
              exists `shouldBe` True