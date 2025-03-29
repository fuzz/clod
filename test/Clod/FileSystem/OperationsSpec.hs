{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystem.OperationsSpec
-- Description : Tests for file system operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the FileSystem.Operations module.

module Clod.FileSystem.OperationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Control.Monad (forM_, void)
import Data.Either (isRight)

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import Clod.Types
import Clod.Effects
import Clod.Capability
import Clod.FileSystem.Operations

-- | Create a test file with specific content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (BS.pack $ map fromIntegral $ fromEnum <$> content)

spec :: Spec
spec = do
  describe "findFilesInDirectory" $ do
    it "finds all files in a directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a directory structure
        let files = [ "file1.txt", "file2.md", "subdir/file3.txt", "subdir/nested/file4.js" ]
        forM_ files $ \file -> do
          createTestFile (tmpDir </> file) ("Content of " ++ file)
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          fs <- findFilesInDirectory readCap tmpDir
          return (length fs)
        
        -- Verify we found all files (should be 4)
        isRight result `shouldBe` True
        case result of
          Right count -> count `shouldBe` 4
          Left err -> expectationFailure $ "Failed with error: " ++ show err
    
    it "respects capability restrictions" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a directory structure with restricted and allowed subdirectories
        let allowedDir = tmpDir </> "allowed"
            restrictedDir = tmpDir </> "restricted"
            
            allowedFiles = [ "file1.txt", "subdir/file2.txt" ]
            restrictedFiles = [ "secret1.txt", "subdir/secret2.txt" ]
        
        -- Create files
        forM_ allowedFiles $ \file -> do
          createTestFile (allowedDir </> file) ("Allowed: " ++ file)
        
        forM_ restrictedFiles $ \file -> do
          createTestFile (restrictedDir </> file) ("Restricted: " ++ file)
        
        -- Set up capabilities that only allow access to the allowed directory
        let readCap = fileReadCap [allowedDir]
        
        -- Run the test to find files in the allowed directory
        allowedResult <- runM . runError . runFileSystemIO $ do
          findFilesInDirectory readCap allowedDir
        
        -- Run the test to find files in the restricted directory (should fail)
        restrictedResult <- runM . runError . runFileSystemIO $ do
          findFilesInDirectory readCap restrictedDir
        
        -- Verify results
        isRight allowedResult `shouldBe` True
        case allowedResult of
          Right files -> length files `shouldBe` 2
          Left err -> expectationFailure $ "Failed with error for allowed dir: " ++ show err
        
        case restrictedResult of
          Left (ConfigError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed due to restricted access"
  
  describe "copyFileWithCapability" $ do
    it "can copy a file with proper capabilities" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up source and destination
        let srcFile = tmpDir </> "source.txt"
            destFile = tmpDir </> "destination.txt"
            content = "Test content for copying"
        
        -- Create source file
        createTestFile srcFile content
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          copyFileWithCapability readCap writeCap srcFile destFile
          exists <- safeFileExists readCap destFile
          return exists
        
        -- Verify the file was copied
        isRight result `shouldBe` True
        case result of
          Right exists -> exists `shouldBe` True
          Left err -> expectationFailure $ "Failed with error: " ++ show err
        
        -- Verify the content is correct
        destContent <- BS.readFile destFile
        map toEnum (BS.unpack destContent) `shouldBe` content
    
    it "respects capability restrictions when copying" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up directories
        let srcDir = tmpDir </> "source"
            destDir = tmpDir </> "destination"
            restrictedDir = tmpDir </> "restricted"
            
            srcFile = srcDir </> "file.txt"
            destFile = destDir </> "file.txt"
            restrictedFile = restrictedDir </> "file.txt"
            
        -- Create directories and source file
        forM_ [srcDir, destDir, restrictedDir] $ createDirectoryIfMissing True
        createTestFile srcFile "Source content"
        
        -- Set up capabilities that don't include restricted dir
        let readCap = fileReadCap [srcDir, destDir]
            writeCap = fileWriteCap [destDir]
            
        -- Test valid copy
        validResult <- runM . runError . runFileSystemIO $ do
          copyFileWithCapability readCap writeCap srcFile destFile
          safeFileExists readCap destFile
        
        -- Test copy to restricted location
        restrictedResult <- runM . runError . runFileSystemIO $ do
          copyFileWithCapability readCap writeCap srcFile restrictedFile
        
        -- Verify results
        isRight validResult `shouldBe` True
        case validResult of
          Right exists -> exists `shouldBe` True
          Left err -> expectationFailure $ "Failed with error for valid copy: " ++ show err
        
        case restrictedResult of
          Left (ConfigError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed due to restricted destination"