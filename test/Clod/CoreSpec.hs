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
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, 
                           doesFileExist, getDirectoryContents)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.Either (isRight)
import Control.Monad (when)
import Data.List (isInfixOf)

import Clod.Core
import Clod.Types 
  ( ClodConfig(..), FileResult(..), FileReadCap(..), IgnorePattern(..),
    runClodM, isPathAllowed, fileReadCap, fileWriteCap
  )

-- | Test specification for Core module
spec :: Spec
spec = do
  fileProcessingSpec
  runClodAppSpec
  ignorePatternSpec
  
-- | Tests for the file processing functionality
fileProcessingSpec :: Spec
fileProcessingSpec = describe "File processing with ClodM" $ do
  it "can process text files with capabilities" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directories and files
      createDirectoryIfMissing True (tmpDir </> "src")
      createDirectoryIfMissing True (tmpDir </> "staging")
      writeFile (tmpDir </> "src" </> "test.txt") "Test file"
      
      -- Create a test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = []
          }
      
      -- Create limited capabilities (only src directory for read, staging for write)
      let readCap = fileReadCap [tmpDir </> "src"]
          writeCap = fileWriteCap [tmpDir </> "staging"]
      
      -- Process a file
      result <- runClodM config $
        processFile readCap writeCap (tmpDir </> "src" </> "test.txt") "src/test.txt"
      
      -- Should succeed
      case result of
        Left err -> expectationFailure $ "Failed to process file: " ++ show err
        Right fileResult -> fileResult `shouldBe` Success
      
      -- Verify the file was copied to staging
      copiedExists <- doesFileExist (tmpDir </> "staging" </> "test.txt")
      copiedExists `shouldBe` True
  
  it "respects capability restrictions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create directories
      createDirectoryIfMissing True (tmpDir </> "src")
      createDirectoryIfMissing True (tmpDir </> "private")
      createDirectoryIfMissing True (tmpDir </> "staging")
      
      -- Create test files
      writeFile (tmpDir </> "src" </> "test.txt") "Public test file"
      writeFile (tmpDir </> "private" </> "secret.txt") "Secret file"
      
      -- Create a test config
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = []
          }
      
      -- Create limited capabilities (only src directory, not private)
      let readCap = fileReadCap [tmpDir </> "src"]
          writeCap = fileWriteCap [tmpDir </> "staging"]
      
      -- Try to access file outside allowed directory
      result <- runClodM config $
        processFile readCap writeCap (tmpDir </> "private" </> "secret.txt") "private/secret.txt"
      
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
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = []
          }
          
      -- Run initialization function
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
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = []
          }
          
      -- Run application
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
      let _config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = []
          }
      
      -- Test that our capability restricts access as expected
      let readCap = fileReadCap [tmpDir]
      
      -- Try to check if a file exists outside our capability
      allowed <- isPathAllowed (allowedReadDirs readCap) forbiddenDir
      allowed `shouldBe` False

-- | Tests for ignore pattern handling
ignorePatternSpec :: Spec
ignorePatternSpec = describe "Ignore pattern handling" $ do
  it "respects .clodignore patterns" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create directories and test files
      createDirectoryIfMissing True (tmpDir </> ".git")
      createDirectoryIfMissing True (tmpDir </> "src")
      createDirectoryIfMissing True (tmpDir </> "node_modules")
      createDirectoryIfMissing True (tmpDir </> "staging")
      
      -- Create test files
      writeFile (tmpDir </> "src" </> "main.js") "console.log('test');"
      writeFile (tmpDir </> "node_modules" </> "package.js") "module.exports = {};"
      writeFile (tmpDir </> ".git" </> "HEAD") "ref: refs/heads/main"
      
      -- Create a .clodignore file (both in root and .clod dir for compatibility)
      createDirectoryIfMissing True (tmpDir </> ".clod")
      writeFile (tmpDir </> ".clodignore") "node_modules/\n.git/"
      writeFile (tmpDir </> ".clod" </> ".clodignore") "node_modules/\n.git/"
      
      -- Create a fake git repo to ensure Git detection works
      createDirectoryIfMissing True (tmpDir </> ".git" </> "objects")
      createDirectoryIfMissing True (tmpDir </> ".git" </> "refs")
      writeFile (tmpDir </> ".git" </> "config") 
        "[core]\n\trepositoryformatversion = 0\n\tfilemode = true\n\tbare = false\n"
      
      -- Create a test config that sets the ignorePatterns manually
      -- This is critical - we need to set the patterns explicitly
      let config = ClodConfig {
            projectPath = tmpDir,
            stagingDir = tmpDir </> "staging",
            configDir = tmpDir </> ".clod", 
            databaseFile = tmpDir </> ".clod" </> "database.dhall",
            previousStaging = Nothing,
            flushMode = False,
            lastMode = False,
            timestamp = "20250325",
            currentStaging = tmpDir </> "staging",
            testMode = True,
            verbose = False,
            ignorePatterns = [IgnorePattern "node_modules/", IgnorePattern ".git/"]  -- Set patterns directly
          }
      
      -- Call Core.runClodApp directly, which should use the ignorePatterns in the config
      result <- runClodApp config "" False True False
      
      -- Check if it worked
      case result of
        Left err -> expectationFailure $ "Failed to run clod: " ++ show err
        Right _ -> do
          -- Directly check if the ignored files were copied
          let nodeModulesJs = tmpDir </> "staging" </> "node_modules-package.js"
              gitFile = tmpDir </> "staging" </> ".git-HEAD"
              manifestPath = tmpDir </> "staging" </> "_path_manifest.json"
          
          -- Get all files in the staging directory
          allFiles <- getDirectoryContents (tmpDir </> "staging")
          
          -- Try to find the main.js file regardless of its exact name
          -- This makes the test more resilient to different optimized filename implementations
          let mainFiles = filter (\f -> "main" `isInfixOf` f && f `notElem` [".", ".."]) allFiles
          let hasMainFile = not (null mainFiles)
          
          -- Check if node_modules or git files were copied (they shouldn't be)
          nodeModulesCopied <- doesFileExist nodeModulesJs
          gitFileCopied <- doesFileExist gitFile
          
          -- Check if the manifest exists
          manifestExists <- doesFileExist manifestPath
          
          -- Run assertions
          -- The src/main.js file should be copied (or any file with 'main' in the name)
          hasMainFile `shouldBe` True
          
          -- The node_modules/package.js file should NOT be copied (ignored)
          nodeModulesCopied `shouldBe` False
          
          -- The .git/HEAD file should NOT be copied (ignored)
          gitFileCopied `shouldBe` False
          
          -- The manifest should exist
          manifestExists `shouldBe` True
          
          -- Check manifest contents
          when manifestExists $ do
            manifestContent <- readFile manifestPath
            
            -- The manifest should contain src/main.js but not node_modules or .git
            manifestContent `shouldContain` "src/main.js"
            manifestContent `shouldNotContain` "node_modules"
            manifestContent `shouldNotContain` ".git"