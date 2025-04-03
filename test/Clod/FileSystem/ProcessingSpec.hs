{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Clod.FileSystem.ProcessingSpec
-- Description : Tests for file processing functions
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for file processing functionality.

module Clod.FileSystem.ProcessingSpec (spec) where

import Test.Hspec
import System.FilePath ((</>))
import System.Directory (doesFileExist) 
import System.IO.Temp (withSystemTempDirectory)

import Clod.Types
import Clod.FileSystem.Processing
import Clod.TestHelpers (defaultTestConfig)

-- | Test specification for FileSystem.Processing module
spec :: Spec
spec = do
  writeManifestFileSpec
  createOptimizedNameSpec

-- | Tests for manifest file creation
writeManifestFileSpec :: Spec
writeManifestFileSpec = describe "writeManifestFile" $ do
  it "writes entries to the manifest file" $ do
    -- Create a temp directory
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a manifest file path and entries
      let manifestPath = tmpDir </> "manifest.dhall"
          entry1 = (OptimizedName "src-main.js", OriginalPath "src/main.js")
          entry2 = (OptimizedName "src-utils.js", OriginalPath "src/utils.js")
          entries = [entry1, entry2]
          config = defaultTestConfig tmpDir
      
      -- Create a write capability for the temp directory
      let writeCap = fileWriteCap [tmpDir]
      
      -- Write the manifest
      result <- runClodM config $ writeManifestFile writeCap manifestPath entries
      
      -- Verify result
      case result of
        Left err -> expectationFailure $ "Failed to write manifest: " ++ show err
        Right _ -> do
          -- Check file exists
          fileExists <- doesFileExist manifestPath
          fileExists `shouldBe` True
          
          -- Check content
          content <- readFile manifestPath
          content `shouldContain` "`src-main.js` = \"src/main.js\""
          content `shouldContain` "`src-utils.js` = \"src/utils.js\""

-- | Tests for optimized name creation
createOptimizedNameSpec :: Spec
createOptimizedNameSpec = describe "createOptimizedName" $ do
  it "flattens directory structure" $ do
    let path = "src/components/Button.jsx"
        optimized = createOptimizedName path
    
    unOptimizedName optimized `shouldBe` "src-components-Button.jsx"
  
  it "handles files in the root directory" $ do
    let path = "README.md"
        optimized = createOptimizedName path
    
    unOptimizedName optimized `shouldBe` "README.md"
  
  it "transforms hidden files" $ do
    let path = ".gitignore"
        optimized = createOptimizedName path
    
    unOptimizedName optimized `shouldBe` "dot--gitignore"
  
  it "works with nested hidden directories" $ do
    let path = ".github/workflows/deploy.yml"
        optimized = createOptimizedName path
    
    unOptimizedName optimized `shouldBe` "dot--github-workflows-deploy.yml"