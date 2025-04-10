{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Clod.FileSystem.OperationsSpec
-- Description : Tests for file system operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for file system operations functionality.

module Clod.FileSystem.OperationsSpec (spec) where

import Test.Hspec
-- import Control.Monad (forM_, void)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Data.List (sort)

import Clod.Types
import Clod.FileSystem.Operations
import Clod.TestHelpers (defaultTestConfig)

-- | Test specification for FileSystem.Operations module
spec :: Spec
spec = do
  findAllFilesSpec
  safeFileOperationsSpec

-- | Tests for findAllFiles function
findAllFilesSpec :: Spec
findAllFilesSpec = describe "findAllFiles" $ do
  it "finds all files in a directory tree" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      let dir = tmpDir </> "project"
      
      -- Create a directory structure
      createDirectoryIfMissing True (dir </> "subdir")
      createDirectoryIfMissing True (dir </> "subdir2")
      
      -- Create files
      BS.writeFile (dir </> "file1.txt") "test"
      BS.writeFile (dir </> "subdir" </> "file2.txt") "test"
      BS.writeFile (dir </> "subdir2" </> "file3.txt") "test"
      
      -- Run the function
      let config = defaultTestConfig dir
      
      result <- runClodM config $ findAllFiles dir [""]
      
      -- Verify result
      case result of
        Left err -> expectationFailure $ "Failed to find files: " ++ show err
        Right files -> do
          let expectedFiles = sort ["file1.txt", "subdir/file2.txt", "subdir2/file3.txt"]
              resultFiles = sort files
          
          -- Check files were found
          resultFiles `shouldBe` expectedFiles

-- | Tests for safe file operations
safeFileOperationsSpec :: Spec
safeFileOperationsSpec = describe "Safe file operations" $ do
  it "can safely read a file within the capability scope" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a file in the temp directory
      let content = "test content"
          filePath = tmpDir </> "test.txt"
      
      BS.writeFile filePath (BS.pack $ map (fromIntegral . fromEnum) content)
      
      -- Create a read capability restricted to the temp directory
      let readCap = fileReadCap [tmpDir]
      let config = defaultTestConfig tmpDir
      
      -- Try to read the file
      result <- runClodM config $ safeReadFile readCap filePath
      
      -- Verify result
      case result of
        Left err -> expectationFailure $ "Failed to read file: " ++ show err
        Right fileContent -> fileContent `shouldBe` BS.pack (map (fromIntegral . fromEnum) content)
  
  it "cannot read a file outside the capability scope" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      let outsideTmpDir = "/tmp/outside"  -- This is outside our temp directory
          readCap = fileReadCap [tmpDir]
          config = defaultTestConfig tmpDir
      
      -- Try to read a file outside the capability scope
      result <- runClodM config $ safeReadFile readCap outsideTmpDir
      
      -- Should fail with capability error
      case result of
        Left (CapabilityError _ _) -> return ()  -- Expected error
        Left other -> expectationFailure $ "Got wrong error: " ++ show other
        Right _ -> expectationFailure "Read succeeded when it should have failed"