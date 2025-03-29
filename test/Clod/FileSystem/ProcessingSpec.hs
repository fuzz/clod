{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystem.ProcessingSpec
-- Description : Tests for file processing operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the FileSystem.Processing module.

module Clod.FileSystem.ProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Control.Monad (forM_, void)
import Data.Either (isRight)
import Data.Time.Clock (getCurrentTime, addUTCTime)

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import Clod.Types
import Clod.Effects
import Clod.Capability
import Clod.FileSystem.Processing

-- | Create a test file with specific content
createTestFile :: FilePath -> String -> IO ()
createTestFile path content = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (BS.pack $ map fromIntegral $ fromEnum <$> content)

spec :: Spec
spec = do
  describe "getModifiedFilesSince" $ do
    it "correctly identifies files modified since a given time" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Get the current time
        now <- getCurrentTime
        
        -- Create older files (modified before the reference time)
        let olderFiles = [ "older1.txt", "older2.md", "older/file3.txt" ]
        forM_ olderFiles $ \file -> do
          createTestFile (tmpDir </> file) ("Older file: " ++ file)
        
        -- Set the reference time to now
        let refTime = now
        
        -- Create newer files (modified after the reference time)
        threadDelay 1000000  -- 1 second delay to ensure file timestamps are different
        let newerFiles = [ "newer1.txt", "newer2.md", "newer/file3.txt" ]
        forM_ newerFiles $ \file -> do
          createTestFile (tmpDir </> file) ("Newer file: " ++ file)
        
        -- Set up capability
        let readCap = fileReadCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          getModifiedFilesSince readCap tmpDir refTime
        
        -- Verify we found only the newer files
        isRight result `shouldBe` True
        case result of
          Right files -> length files `shouldBe` length newerFiles
          Left err -> expectationFailure $ "Failed with error: " ++ show err
    
    it "respects capability restrictions" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create directories
        let allowedDir = tmpDir </> "allowed"
            restrictedDir = tmpDir </> "restricted"
        
        createDirectoryIfMissing True allowedDir
        createDirectoryIfMissing True restrictedDir
        
        -- Get current time
        now <- getCurrentTime
        
        -- Create some files
        createTestFile (allowedDir </> "file.txt") "Allowed file"
        createTestFile (restrictedDir </> "file.txt") "Restricted file"
        
        -- Set up capability that only allows access to the allowed directory
        let readCap = fileReadCap [allowedDir]
        
        -- Run the test on allowed directory
        allowedResult <- runM . runError . runFileSystemIO $ do
          getModifiedFilesSince readCap allowedDir now
        
        -- Run the test on restricted directory
        restrictedResult <- runM . runError . runFileSystemIO $ do
          getModifiedFilesSince readCap restrictedDir now
        
        -- Verify results
        isRight allowedResult `shouldBe` True
        case restrictedResult of
          Left (ConfigError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed due to restricted access"
  
  describe "getTextFiles" $ do
    it "correctly identifies text files" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create text and binary files
        let textFiles = [ "text1.txt", "text2.md", "code.hs" ]
            binaryFiles = [ "binary1.bin", "binary2.exe" ]
        
        -- Create text files
        forM_ textFiles $ \file -> do
          createTestFile (tmpDir </> file) ("Text file: " ++ file)
        
        -- Create binary files with some null bytes
        forM_ binaryFiles $ \file -> do
          BS.writeFile (tmpDir </> file) (BS.pack [0, 1, 2, 3, 0, 5, 6])
        
        -- Set up capability
        let readCap = fileReadCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          allFiles <- findAllFiles readCap tmpDir
          textOnly <- getTextFiles readCap allFiles
          return (length textOnly, length allFiles)
        
        -- Verify results
        isRight result `shouldBe` True
        case result of
          Right (textCount, totalCount) -> do
            textCount `shouldBe` length textFiles
            totalCount `shouldBe` (length textFiles + length binaryFiles)
          Left err -> expectationFailure $ "Failed with error: " ++ show err
      
  describe "saveFilesToStaging" $ do
    it "correctly saves files to the staging directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create source and staging directories
        let sourceDir = tmpDir </> "source"
            stagingDir = tmpDir </> "staging"
        
        createDirectoryIfMissing True sourceDir
        createDirectoryIfMissing True stagingDir
        
        -- Create some files
        let files = [ "file1.txt", "file2.md", "subdir/file3.txt" ]
        forM_ files $ \file -> do
          createTestFile (sourceDir </> file) ("Content of " ++ file)
        
        -- Set up capabilities
        let readCap = fileReadCap [sourceDir]
            writeCap = fileWriteCap [stagingDir]
        
        -- Create the config
        let config = ClodConfig 
              { projectDir = sourceDir
              , stagingDir = stagingDir
              , currentStaging = stagingDir
              , lastRunFile = tmpDir </> ".clod-last-run"
              , ignorePatterns = []
              , useGit = False
              }
        
        -- Run the test
        result <- runM . runError . runReader config . runFileSystemIO $ do
          filePaths <- findAllFiles readCap sourceDir
          let relativePaths = map (makeRelative sourceDir) filePaths
          saveFilesToStaging readCap writeCap filePaths relativePaths
        
        -- Verify results
        isRight result `shouldBe` True
        
        -- Check if files were copied correctly
        forM_ files $ \file -> do
          let stagingFile = stagingDir </> file
          exists <- doesFileExist stagingFile
          exists `shouldBe` True