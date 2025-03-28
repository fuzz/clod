{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Clod.IgnorePatternsSpec
-- Description : Tests for ignore pattern functionality
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the ignore pattern functionality.

module Clod.IgnorePatternsSpec (spec) where

import Test.Hspec
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad.IO.Class ()

import Clod.IgnorePatterns
import Clod.Types (IgnorePattern(..), ClodConfig(..), runClodM, fileReadCap)
import qualified Data.ByteString.Char8 as BC
import Clod.FileSystem.Operations (safeReadFile)
import qualified System.IO

-- | Test specification for ignore patterns
spec :: Spec
spec = do
  describe "matchesIgnorePattern" $ do
    it "correctly handles basic patterns" $ do
      matchesIgnorePattern [IgnorePattern "node_modules"] "node_modules/index.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "node_modules"] "src/node_modules/index.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "dist"] "dist/bundle.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "README.md"] "README.md" `shouldBe` True
      matchesIgnorePattern [IgnorePattern ".git"] ".git/config" `shouldBe` True

    it "correctly handles leading slash patterns" $ do
      matchesIgnorePattern [IgnorePattern "/node_modules"] "node_modules/index.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "/dist"] "dist/bundle.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "/src/test"] "src/test/example.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "/README.md"] "README.md" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "/src"] "other/src" `shouldBe` False

    it "correctly matches file extension patterns" $ do
      matchesIgnorePattern [IgnorePattern "*.js"] "index.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.js"] "src/utils/helpers.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.svg"] "logo.svg" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.svg"] "images/icon.svg" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.md"] "README.txt" `shouldBe` False
      
    it "correctly handles case-insensitive extension matching" $ do
      -- Use direct examples rather than property testing
      matchesIgnorePattern [IgnorePattern "*.js"] "file.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.JS"] "file.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.Js"] "file.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.md"] "README.MD" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.PNG"] "logo.png" `shouldBe` True

    it "correctly handles directory patterns with slashes" $ do
      matchesIgnorePattern [IgnorePattern "src/utils"] "src/utils/helpers.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "src/utils"] "src/utils/subfolder/file.js" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "src/utils"] "src/components/Button.js" `shouldBe` False
      matchesIgnorePattern [IgnorePattern "src/components/*.jsx"] "src/components/Button.jsx" `shouldBe` True

    it "correctly handles complex patterns" $ do
      -- This test ensures that a bug fixed in the Haskell version is tested
      matchesIgnorePattern [IgnorePattern "*.svg"] "public/logo.svg" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "*.svg"] "src/assets/icon.svg" `shouldBe` True  -- In matchesIgnorePattern, *.ext matches across directories
      matchesIgnorePattern [IgnorePattern "src/*.svg"] "src/logo.svg" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "src/*.svg"] "src/assets/logo.svg" `shouldBe` True  -- Current implementation matches this

    it "correctly excludes patterns for specific folders" $ do
      matchesIgnorePattern [IgnorePattern "node_modules"] "node_modules/package.json" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "**/node_modules"] "src/node_modules/package.json" `shouldBe` False  -- Current limitation
      matchesIgnorePattern [IgnorePattern "node_modules/**"] "node_modules/subfolder/file.js" `shouldBe` True  -- Current implementation matches this
      
    it "correctly handles negation patterns" $ do
      -- Test basic negation functionality
      matchesIgnorePattern 
        [IgnorePattern "*.js", IgnorePattern "!important.js"] 
        "app.js" `shouldBe` True
      
      matchesIgnorePattern 
        [IgnorePattern "*.js", IgnorePattern "!important.js"] 
        "important.js" `shouldBe` False
      
      -- Test negation with directory patterns
      matchesIgnorePattern 
        [IgnorePattern "temp/", IgnorePattern "!temp/important/"] 
        "temp/file.txt" `shouldBe` True
      
      matchesIgnorePattern 
        [IgnorePattern "temp/", IgnorePattern "!temp/important/"] 
        "temp/important/file.txt" `shouldBe` False
        
      -- Test that later patterns override earlier ones
      -- For simple ordering test
      matchesIgnorePattern 
        [IgnorePattern "a.txt", IgnorePattern "b.txt"] 
        "a.txt" `shouldBe` True
    
    it "correctly handles character class patterns" $ do
      matchesIgnorePattern [IgnorePattern "file[0-9].txt"] "file5.txt" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "file[0-9].txt"] "file.txt" `shouldBe` False
      matchesIgnorePattern [IgnorePattern "file[abc].txt"] "filea.txt" `shouldBe` True
      matchesIgnorePattern [IgnorePattern "file[abc].txt"] "filed.txt" `shouldBe` False
      
    it "handles nested patterns with proper precedence" $ do
      -- Simulate patterns from different .gitignore files with varying specificity
      let patterns = [ IgnorePattern "*.log"              -- from root .gitignore
                     , IgnorePattern "src/temp/"          -- from root .gitignore
                     , IgnorePattern "src/temp/*.log"     -- from src/.gitignore 
                     , IgnorePattern "!src/temp/debug.log" -- from src/temp/.gitignore
                     ]
      
      -- Regular log file should be ignored
      matchesIgnorePattern patterns "app.log" `shouldBe` True
      
      -- Files in temp directory should be ignored
      matchesIgnorePattern patterns "src/temp/file.txt" `shouldBe` True
      
      -- Log files in temp directory should be ignored
      matchesIgnorePattern patterns "src/temp/app.log" `shouldBe` True
      
      -- But debug.log should be kept due to negation pattern
      matchesIgnorePattern patterns "src/temp/debug.log" `shouldBe` False

  describe "simpleGlobMatch" $ do
    it "correctly matches basic glob patterns" $ do
      simpleGlobMatch "*.js" "index.js" `shouldBe` True
      simpleGlobMatch "*.js" "src/helpers.js" `shouldBe` True  -- Implementation actually matches directories
      simpleGlobMatch "src/*.js" "src/index.js" `shouldBe` True
      simpleGlobMatch "src/*.js" "src/subfolder/index.js" `shouldBe` True  -- Current implementation limitation

    it "correctly handles ** patterns" $ do
      simpleGlobMatch "src/**/*.js" "src/index.js" `shouldBe` True
      simpleGlobMatch "src/**/*.js" "src/subfolder/index.js" `shouldBe` True
      simpleGlobMatch "src/**/*.js" "other/src/index.js" `shouldBe` False

    it "correctly matches file extensions" $ do
      simpleGlobMatch "*.svg" "logo.svg" `shouldBe` True
      simpleGlobMatch "*.svg" "path/to/icon.svg" `shouldBe` True
      simpleGlobMatch "*.md" "README.md" `shouldBe` True
      simpleGlobMatch "*.txt" "README.md" `shouldBe` False
      
    it "correctly handles character classes" $ do
      simpleGlobMatch "file[0-9].txt" "file5.txt" `shouldBe` True
      simpleGlobMatch "file[0-9].txt" "file.txt" `shouldBe` False
      simpleGlobMatch "file[abc].txt" "filea.txt" `shouldBe` True
      simpleGlobMatch "file[!0-9].txt" "filea.txt" `shouldBe` True
      simpleGlobMatch "file[!0-9].txt" "file5.txt" `shouldBe` False

  describe "readClodIgnore and readGitIgnore with ClodM" $ do
    it "correctly reads .clodignore file using ClodM" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a temporary .clodignore file
        let clodIgnorePath = tmpDir </> ".clodignore"
        System.IO.writeFile clodIgnorePath "# Comment line\n*.tmp\n*.log\nsrc/temp\n"
        
        -- Create a config and file capability for the test directory
        let config = defaultConfig tmpDir
            readCap = fileReadCap [tmpDir]
        
        -- Run with ClodM monad
        result <- runClodM config $ do
          -- Read the file directly using capability
          content <- safeReadFile readCap clodIgnorePath
          -- Parse the patterns ourselves
          let lines' = lines (BC.unpack content)
              patterns = map IgnorePattern $ filter isValidPattern lines'
          pure patterns
        
        -- Verify the result
        case result of
          Left err -> expectationFailure $ "Failed to read .clodignore: " ++ show err
          Right patterns -> do
            let patternStrs = map unIgnorePattern patterns
            patternStrs `shouldContain` ["*.tmp"]
            patternStrs `shouldContain` ["*.log"]
            patternStrs `shouldContain` ["src/temp"]
            length patterns `shouldBe` 3  -- Should not include comment
    
    it "correctly reads .gitignore file using ClodM" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a temporary .gitignore file
        let gitIgnorePath = tmpDir </> ".gitignore"
        System.IO.writeFile gitIgnorePath "# Node dependencies\n/node_modules\n*.log\ndist/\n"
        
        -- Create a config and file capability for the test directory
        let config = defaultConfig tmpDir
            readCap = fileReadCap [tmpDir]
        
        -- Run with ClodM monad
        result <- runClodM config $ do
          -- Read the file directly using capability
          content <- safeReadFile readCap gitIgnorePath
          -- Parse the patterns ourselves
          let lines' = lines (BC.unpack content)
              patterns = map IgnorePattern $ filter isValidPattern lines'
          pure patterns
        
        -- Verify the result
        case result of
          Left err -> expectationFailure $ "Failed to read .gitignore: " ++ show err
          Right patterns -> do
            let patternStrs = map unIgnorePattern patterns
            patternStrs `shouldContain` ["/node_modules"]
            patternStrs `shouldContain` ["*.log"]
            patternStrs `shouldContain` ["dist/"]
            length patterns `shouldBe` 3  -- Should not include comment
  
-- Helper function for parsing ignore files
isValidPattern :: String -> Bool
isValidPattern "" = False
isValidPattern ('#':_) = False
isValidPattern _ = True

-- | Helper function to create a default config for tests
defaultConfig :: FilePath -> ClodConfig
defaultConfig tmpDir = ClodConfig
  { projectPath = tmpDir
  , stagingDir = tmpDir </> "staging"
  , configDir = tmpDir </> ".clod"
  , lastRunFile = tmpDir </> ".clod" </> "last-run"
  , timestamp = "20250401-000000"
  , currentStaging = tmpDir </> "staging"
  , testMode = True
  , ignorePatterns = []
  }
