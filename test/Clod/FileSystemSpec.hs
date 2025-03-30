{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystemSpec
-- Description : Tests for file system operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for file system operations.

module Clod.FileSystemSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.Either (isRight)
import qualified System.IO
import Prelude hiding (readFile, writeFile)
import Control.Monad.IO.Class ()

import Clod.Types (ClodConfig(..), runClodM, fileReadCap, fileWriteCap)
import Clod.FileSystem.Operations (safeReadFile, safeCopyFile)
import Clod.FileSystem.Detection (safeFileExists)

-- | Test specification for file system operations
spec :: Spec
spec = do
  describe "File system operations with capabilities" $ do
    it "restricts file access to allowed directories" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test directories and files
        createDirectoryIfMissing True (tmpDir </> "allowed")
        createDirectoryIfMissing True (tmpDir </> "forbidden")
        
        System.IO.writeFile (tmpDir </> "allowed" </> "test.txt") "allowed content"
        System.IO.writeFile (tmpDir </> "forbidden" </> "secret.txt") "secret content"
        
        -- Create capability that only allows access to the "allowed" directory
        let config = defaultConfig tmpDir
            readCap = fileReadCap [tmpDir </> "allowed"]
        
        -- Attempt to read from allowed directory
        result1 <- runClodM config $
          safeReadFile readCap (tmpDir </> "allowed" </> "test.txt")
        
        -- Attempt to read from forbidden directory
        result2 <- runClodM config $
          safeReadFile readCap (tmpDir </> "forbidden" </> "secret.txt")
        
        -- Check results
        result1 `shouldSatisfy` isRight
        case result2 of
          Left _ -> return () -- Expected to fail with access denied
          Right _ -> expectationFailure "Access to forbidden directory was allowed"
  
  describe "File system operations" $ do
    it "can find files in a directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test directory structure
        createDirectoryIfMissing True (tmpDir </> "src" </> "components")
        createDirectoryIfMissing True (tmpDir </> "test")
        
        -- Create some test files
        System.IO.writeFile (tmpDir </> "README.md") "# Test"
        System.IO.writeFile (tmpDir </> "src" </> "index.js") "console.log('hello');"
        System.IO.writeFile (tmpDir </> "src" </> "components" </> "Button.jsx") "<Button />"
        System.IO.writeFile (tmpDir </> "test" </> "index.test.js") "test('example');"
        
        -- Build a list of files manually using a helper function
        let config = defaultConfig tmpDir
            readCap = fileReadCap [tmpDir]
            
        -- Check if each file exists using the capability-based system
        readme <- runClodM config $ safeFileExists readCap (tmpDir </> "README.md")
        indexJs <- runClodM config $ safeFileExists readCap (tmpDir </> "src" </> "index.js")
        buttonJsx <- runClodM config $ safeFileExists readCap (tmpDir </> "src" </> "components" </> "Button.jsx")
        testJs <- runClodM config $ safeFileExists readCap (tmpDir </> "test" </> "index.test.js")
        
        -- Verify the results
        readme `shouldBe` Right True
        indexJs `shouldBe` Right True
        buttonJsx `shouldBe` Right True
        testJs `shouldBe` Right True
            
    it "properly handles file copying" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create source and destination directories
        createDirectoryIfMissing True (tmpDir </> "source")
        createDirectoryIfMissing True (tmpDir </> "dest")
        
        -- Create a test file
        System.IO.writeFile (tmpDir </> "source" </> "test.txt") "test content"
        
        -- Create capabilities
        let config = defaultConfig tmpDir
            readCap = fileReadCap [tmpDir </> "source"]
            writeCap = fileWriteCap [tmpDir </> "dest"]
            
        -- Copy file using the capability-based system
        result <- runClodM config $ 
          safeCopyFile readCap writeCap (tmpDir </> "source" </> "test.txt") (tmpDir </> "dest" </> "test.txt")
          
        -- Verify the copy worked
        case result of
          Left err -> expectationFailure $ "Failed to copy file: " ++ show err
          Right _ -> do
            exists <- doesFileExist (tmpDir </> "dest" </> "test.txt")
            exists `shouldBe` True
            
            content <- System.IO.readFile (tmpDir </> "dest" </> "test.txt")
            content `shouldBe` "test content"
  
-- | Helper function to create a default config for tests
defaultConfig :: FilePath -> ClodConfig
defaultConfig tmpDir = ClodConfig
  { projectPath = tmpDir
   stagingDir = tmpDir </> "staging"
   configDir = tmpDir </> ".clod"
   databaseFile = tmpDir </> ".clod" </> "database.dhall",
  previousStaging = Nothing,
  flushMode = False,
  lastMode = False,
   timestamp = "20250401-000000"
   currentStaging = tmpDir </> "staging"
   testMode = True,
             verbose = False
   ignorePatterns = []
  }
