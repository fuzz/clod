{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystem.DetectionSpec
-- Description : Tests for file type detection
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for file type detection functionality 
-- using magic-based file type detection.

module Clod.FileSystem.DetectionSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
-- import qualified System.IO
import qualified Data.ByteString as BS

import Clod.Types (runClodM, fileReadCap, configDir, (&), (.~))
import Clod.FileSystem.Detection (safeIsTextFile, isTextDescription, needsTransformation)
import Clod.TestHelpers (defaultTestConfig)

-- | Our own FileType definition for testing purposes
data FileType = TextFile | BinaryFile
  deriving (Show, Eq)

-- | Test specification for FileSystem.Detection module
spec :: Spec
spec = do
  magicDetectionSpec
  mimeTypeSpec
  transformationSpec

-- | Tests for magic-based file type detection
magicDetectionSpec :: Spec
magicDetectionSpec = describe "Magic-based file type detection" $ do
  it "correctly detects text files using magic" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a plain text file - this will reliably be detected as text/plain
      let textFile = tmpDir </> "text.txt"
      writeFile textFile "This is plain text content with multiple lines.\nSecond line\nThird line"
      
      -- Create a config and capabilities
      let config = defaultTestConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Test the text file
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap textFile
        return (if isText then TextFile else BinaryFile)
      
      case result of
        Left err -> expectationFailure $ "Error detecting text file: " ++ show err
        Right fileType -> fileType `shouldBe` TextFile

  it "correctly identifies binary files" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create binary files with appropriate content
      let pngFile = tmpDir </> "image.png"
      let exeFile = tmpDir </> "program.exe"
      
      -- Write some binary data (PNG header and some random bytes)
      BS.writeFile pngFile $ BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x0D]
      
      -- Write EXE header
      BS.writeFile exeFile $ BS.pack [0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00]
      
      -- Create a config and capabilities
      let config = defaultTestConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Test each file
      forM_ [pngFile, exeFile] $ \file -> do
        result <- runClodM config $ do
          isText <- safeIsTextFile readCap file
          return (if isText then TextFile else BinaryFile)
        
        case result of
          Left err -> expectationFailure $ "Error detecting binary file: " ++ show err
          Right fileType -> fileType `shouldBe` BinaryFile

-- | Create a test environment for testing with embedded patterns
setupTestEnvironment :: FilePath -> IO ()
setupTestEnvironment tmpDir = do
  -- Create the necessary directory structure
  let resourceDir = tmpDir </> "resources"
  createDirectoryIfMissing True resourceDir
  
  -- No need to create a patterns file anymore as we're using
  -- the embedded version directly in the code

-- | Tests for text file detection through description
mimeTypeSpec :: Spec
mimeTypeSpec = describe "File description detection" $ do
  it "identifies text file descriptions correctly" $ do
    -- Prepare a test environment
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Setup test environment
      setupTestEnvironment tmpDir
      
      -- Set config directory to our test directory
      let config = defaultTestConfig tmpDir & configDir .~ tmpDir
      
      -- Test various text file descriptions
      let textDescriptions = 
            [ "ASCII text"
            , "text/plain"
            , "UTF-8 text"
            , "JSON data" 
            , "XML document"
            , "HTML document"
            , "source code"
            , "script file"
            ]
      
      -- For each description, test in our monad
      forM_ textDescriptions $ \desc -> do
        result <- runClodM config $ isTextDescription desc
        case result of
          Left err -> expectationFailure $ "Error checking description: " ++ show err
          Right isText -> isText `shouldBe` True
  
  it "identifies non-text file descriptions correctly" $ do
    -- Prepare a test environment
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Setup test environment
      setupTestEnvironment tmpDir
      
      -- Set config directory to our test directory
      let config = defaultTestConfig tmpDir & configDir .~ tmpDir
      
      -- Test various non-text file descriptions
      let nonTextDescriptions = 
            [ "PNG image data"
            , "JPEG image data"
            , "ELF executable"
            , "Zip archive data"
            , "PDF document"
            , "MPEG audio"
            , "binary data"
            ]
      
      -- For each description, test in our monad
      forM_ nonTextDescriptions $ \desc -> do
        result <- runClodM config $ isTextDescription desc
        case result of
          Left err -> expectationFailure $ "Error checking description: " ++ show err
          Right isText -> isText `shouldBe` False

-- | Tests for special file handling
transformationSpec :: Spec
transformationSpec = describe "Special file handling" $ do
  it "identifies files needing special transformation" $ do
    -- Test files that need special handling
    let specialFiles = 
          [ "/path/to/.gitignore"
          , "/path/to/.env"
          , "/path/to/logo.svg"
          ]
    
    forM_ specialFiles $ \file ->
      needsTransformation file `shouldBe` True
  
  it "identifies files not needing special transformation" $ do
    -- Test regular files
    let regularFiles = 
          [ "/path/to/file.txt"
          , "/path/to/image.png"
          , "/path/to/normal.html"
          ]
    
    forM_ regularFiles $ \file ->
      needsTransformation file `shouldBe` False