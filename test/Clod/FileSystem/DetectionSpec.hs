{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.FileSystem.DetectionSpec
-- Description : Tests for file type detection
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for file type detection functionality 
-- using magic-based file type detection.

module Clod.FileSystem.DetectionSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
-- import qualified System.IO
import qualified Data.ByteString as BS

import Clod.Types (runClodM, fileReadCap)
import Clod.FileSystem.Detection (safeIsTextFile, isMimeTypeText, needsTransformation)
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
      -- Create test files with appropriate content
      let textFile = tmpDir </> "text.txt"
      let htmlFile = tmpDir </> "page.html"
      let jsonFile = tmpDir </> "data.json"
      
      writeFile textFile "This is plain text."
      writeFile htmlFile "<!DOCTYPE html><html><body>HTML content</body></html>"
      writeFile jsonFile "{\"key\": \"value\"}"
      
      -- Create a config and capabilities
      let config = defaultTestConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Test each file
      forM_ [textFile, htmlFile, jsonFile] $ \file -> do
        result <- runClodM config $ do
          isText <- safeIsTextFile readCap file
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

-- | Tests for MIME type determination
mimeTypeSpec :: Spec
mimeTypeSpec = describe "MIME type detection" $ do
  it "identifies text MIME types correctly" $ do
    -- Test various text MIME types
    let textMimes = 
          [ "text/plain"
          , "text/html"
          , "text/css"
          , "text/javascript"
          , "application/json"
          , "application/xml"
          , "application/javascript"
          , "application/x-shell"
          , "application/x-shellscript"
          , "application/x-perl-script"
          ]
    
    forM_ textMimes $ \mime ->
      isMimeTypeText mime `shouldBe` True
  
  it "identifies non-text MIME types correctly" $ do
    -- Test various non-text MIME types
    let nonTextMimes = 
          [ "application/octet-stream"
          , "application/pdf"
          , "application/zip"
          , "image/png"
          , "image/jpeg"
          , "audio/mpeg"
          , "video/mp4"
          ]
    
    forM_ nonTextMimes $ \mime ->
      isMimeTypeText mime `shouldBe` False

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