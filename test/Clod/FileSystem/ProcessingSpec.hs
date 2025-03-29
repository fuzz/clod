{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.ProcessingSpec
-- Description : Tests for the FileSystem.Processing module
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module tests the file processing functionality, including
-- manifest generation and optimization.

module Clod.FileSystem.ProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory (doesFileExist, createDirectory, withCurrentDirectory, 
                         removeFile, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Function (on)
import Data.List (nubBy, sort)
import Data.IORef (newIORef, readIORef)

import Clod.Types
import Clod.FileSystem.Processing

-- | Test the file processing functionality
spec :: Spec
spec = do
  describe "createOptimizedName" $ do
    it "converts directory separators to dashes" $ do
      unOptimizedName (createOptimizedName "src/main.js") `shouldBe` "src-main.js"
      unOptimizedName (createOptimizedName "nested/dirs/file.txt") `shouldBe` "nested-dirs-file.txt"
      
    it "leaves files without directories unchanged" $ do
      unOptimizedName (createOptimizedName "file.txt") `shouldBe` "file.txt"

  describe "escapeJSON" $ do
    it "escapes backslashes and quotes" $ do
      escapeJSON "path/with/\"quotes\"" `shouldBe` "path/with/\\\"quotes\\\""
      escapeJSON "Windows\\style\\path" `shouldBe` "Windows\\\\style\\\\path"
      escapeJSON "Combined \"quotes\" and \\backslashes\\" `shouldBe` "Combined \\\"quotes\\\" and \\\\backslashes\\\\"
      
    it "leaves other characters unchanged" $ do
      escapeJSON "normal text" `shouldBe` "normal text"
      escapeJSON "!@#$%^&*()" `shouldBe` "!@#$%^&*()"

  describe "addToManifest" $ do
    it "adds an entry to the manifest file" $ do
      -- Create a temp directory
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a manifest file
        let manifestPath = tmpDir </> "manifest.json"
            originalPath = OriginalPath "src/main.js"
            optimizedName = OptimizedName "src-main.js"
            config = defaultTestConfig
        
        -- Create a first entry reference
        firstEntryRef <- newIORef True
        
        -- Run the function
        result <- runClodM config $ addToManifest manifestPath originalPath optimizedName firstEntryRef
        result `shouldBe` Right ()
        
        -- Check the manifest file was created
        manifestContent <- readFile manifestPath
        manifestContent `shouldBe` "  \"src-main.js\": \"src/main.js\""
        
        -- Add a second entry
        let secondPath = OriginalPath "src/utils.js"
            secondName = OptimizedName "src-utils.js"
        
        result2 <- runClodM config $ addToManifest manifestPath secondPath secondName firstEntryRef
        result2 `shouldBe` Right ()
        
        -- Check both entries are in the manifest
        manifestContent2 <- readFile manifestPath
        manifestContent2 `shouldBe` "  \"src-main.js\": \"src/main.js\",\n  \"src-utils.js\": \"src/utils.js\""

  describe "processFileManifestOnly" $ do
    it "adds a file to the manifest without copying" $ do
      -- Create a temp directory
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test file and manifest
        let testFile = tmpDir </> "test.js"
            manifestPath = tmpDir </> "manifest.json"
            relPath = "test.js"  -- Relative path from project root
            
        BS.writeFile testFile "console.log('test');"
        
        -- First entry reference
        firstEntryRef <- newIORef True
        
        -- Create config with tmpDir as project path
        let config = ClodConfig 
              { projectPath = tmpDir
              , stagingDir = tmpDir
              , configDir = tmpDir
              , lastRunFile = tmpDir
              , timestamp = ""
              , currentStaging = tmpDir
              , testMode = True
              , ignorePatterns = []
              }
            
        -- Run the function
        result <- runClodM config $ processFileManifestOnly config manifestPath testFile relPath firstEntryRef
        
        -- Check it succeeded
        result `shouldBe` Success
        
        -- Verify the manifest was created
        manifestExists <- doesFileExist manifestPath
        manifestExists `shouldBe` True
        
        -- Check the manifest content
        manifestContent <- readFile manifestPath
        manifestContent `shouldBe` "  \"test.js\": \"test.js\""

  describe "processFile" $ do
    it "copies a file to the staging area and adds to manifest" $ do
      -- Create source and staging directories
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            fullPath = sourceDir </> "test.js"
            relPath = "test.js"
            manifestPath = stagingDir </> "manifest.json"
            content = "console.log('test');"
            
        createDirectory sourceDir
        createDirectory stagingDir
        BS.writeFile fullPath (BS.pack $ map (toEnum . fromEnum) content)
        
        -- First entry reference
        firstEntryRef <- newIORef True
        
        -- Create config with sourceDir as project path and stagingDir
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , ignorePatterns = []
              }
            
        -- Run the function
        result <- runClodM config $ processFile config manifestPath fullPath relPath firstEntryRef
        
        -- Check it succeeded
        result `shouldBe` Success
        
        -- Verify the file was copied to staging
        let stagingFile = stagingDir </> "test.js"
        fileExists <- doesFileExist stagingFile
        fileExists `shouldBe` True
        
        -- Check the content is correct
        copiedContent <- BS.unpack <$> BS.readFile stagingFile
        map (toEnum . fromEnum) copiedContent `shouldBe` content
        
        -- Verify the manifest was created
        manifestExists <- doesFileExist manifestPath
        manifestExists `shouldBe` True
        
        -- Check the manifest content
        manifestContent <- readFile manifestPath
        manifestContent `shouldBe` "  \"test.js\": \"test.js\""
        
    it "skips binary files" $ do
      pending "Pending until we have a reliable binary file detection implementation"

-- | Default test configuration
defaultTestConfig :: ClodConfig
defaultTestConfig = ClodConfig
  { projectPath = "/"
  , stagingDir = "/"
  , configDir = "/"
  , lastRunFile = "/"
  , timestamp = ""
  , currentStaging = "/"
  , testMode = True
  , ignorePatterns = []
  }