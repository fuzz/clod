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
-- This module contains tests for file type detection functionality.

module Clod.FileSystem.DetectionSpec (spec) where

import Test.Hspec
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List ()
import Data.Char ()

import Clod.Types (ClodConfig(..), runClodM, fileReadCap)
import Clod.FileSystem.Detection (safeIsTextFile)
import Control.Monad.IO.Class ()

-- | Our own FileType definition for testing purposes
data FileType = TextFile | BinaryFile
  deriving (Show, Eq)

-- | Test specification for FileSystem.Detection module
spec :: Spec
spec = do
  fileTypeDetectionSpec
  binaryDetectionSpec
  encodingDetectionSpec
  extensionBasedDetectionSpec

-- | Tests for file type detection
fileTypeDetectionSpec :: Spec
fileTypeDetectionSpec = describe "File type detection" $ do
  it "correctly detects text files" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a text file
      let textFile = tmpDir </> "text.txt"
      System.IO.writeFile textFile "This is a text file with plain ASCII content."
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap textFile
        return (if isText then TextFile else BinaryFile)
      
      -- Verify detection
      case result of
        Left err -> expectationFailure $ "Failed to detect file type: " ++ show err
        Right fileType -> fileType `shouldBe` TextFile

  it "correctly detects empty files" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create an empty file
      let emptyFile = tmpDir </> "empty.txt"
      System.IO.writeFile emptyFile ""
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap emptyFile
        return (if isText then TextFile else BinaryFile)
      
      -- Verify detection
      case result of
        Left err -> expectationFailure $ "Failed to detect file type: " ++ show err
        Right fileType -> fileType `shouldBe` TextFile

-- | Tests for binary file detection
binaryDetectionSpec :: Spec
binaryDetectionSpec = describe "Binary file detection" $ do
  it "correctly detects binary files" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a binary file with non-text bytes
      let binaryFile = tmpDir </> "binary.bin"
      BS.writeFile binaryFile $ BS.pack [0x00, 0x01, 0x02, 0x03, 0x7F, 0xFF]
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap binaryFile
        return (if isText then TextFile else BinaryFile)
      
      -- Verify detection
      case result of
        Left err -> expectationFailure $ "Failed to detect file type: " ++ show err
        Right fileType -> fileType `shouldBe` BinaryFile

  it "correctly handles commonly known binary file extensions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create files with binary extensions but text content
      let pdfFile = tmpDir </> "fake.pdf"
      let pngFile = tmpDir </> "fake.png"
      
      -- Write text content to these files
      System.IO.writeFile pdfFile "This is not really a PDF file"
      System.IO.writeFile pngFile "This is not really a PNG file"
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection for both files
      pdfResult <- runClodM config $ do
        isText <- safeIsTextFile readCap pdfFile
        return (if isText then TextFile else BinaryFile)
        
      pngResult <- runClodM config $ do
        isText <- safeIsTextFile readCap pngFile
        return (if isText then TextFile else BinaryFile)
      
      -- Verify detection based on extension 
      case pdfResult of
        Left err -> expectationFailure $ "Failed to detect PDF file type: " ++ show err
        Right fileType -> fileType `shouldBe` BinaryFile
        
      case pngResult of
        Left err -> expectationFailure $ "Failed to detect PNG file type: " ++ show err
        Right fileType -> fileType `shouldBe` BinaryFile

-- | Tests for text encoding detection
encodingDetectionSpec :: Spec
encodingDetectionSpec = describe "Text encoding detection" $ do
  it "correctly handles UTF-8 text files" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a UTF-8 file with non-ASCII characters
      let utf8File = tmpDir </> "utf8.txt"
      System.IO.writeFile utf8File "UTF-8 text with special characters: äöüß éèê 日本語"
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap utf8File
        return (if isText then TextFile else BinaryFile)
      
      -- Verify detection
      case result of
        Left err -> expectationFailure $ "Failed to detect file type: " ++ show err
        Right fileType -> fileType `shouldBe` TextFile

  it "correctly handles files with a few non-text bytes" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create a mostly text file with a few binary bytes
      let mixedFile = tmpDir </> "mixed.txt"
      BS.writeFile mixedFile $ BS.concat [BC.pack "This is mostly text but has a few binary bytes: ", BS.pack [0x00, 0x01, 0x02], BC.pack " and then more text."]
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Run detection
      result <- runClodM config $ do
        isText <- safeIsTextFile readCap mixedFile
        return (if isText then TextFile else BinaryFile)
      
      -- Binary detection should identify this as binary due to null bytes
      case result of
        Left err -> expectationFailure $ "Failed to detect file type: " ++ show err
        Right fileType -> fileType `shouldBe` BinaryFile

-- | Tests for extension-based detection
extensionBasedDetectionSpec :: Spec
extensionBasedDetectionSpec = describe "Extension-based detection" $ do
  it "correctly identifies source code files as text" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create sample source code files
      let files = [ (tmpDir </> "source.js", "// JavaScript file\nconsole.log('hello');")
                   (tmpDir </> "source.py", "# Python file\nprint('hello')")
                   (tmpDir </> "source.hs", "-- Haskell file\nmain = putStrLn \"hello\"")
                   (tmpDir </> "source.c", "// C file\n#include <stdio.h>\nint main() { printf(\"hello\"); return 0; }")
                  ]
                  
      -- Create the files
      mapM_ (\(path, content) -> System.IO.writeFile path content) files
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Test each file
      results <- mapM (\(path, _) -> 
        runClodM config $ do
          isText <- safeIsTextFile readCap path
          return (if isText then TextFile else BinaryFile)) files
      
      -- Verify all are detected as text files
      let isCorrectType (Right TextFile) = True
          isCorrectType _ = False
      all isCorrectType results `shouldBe` True
      
  it "correctly handles common special-case extensions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create special case files (these might be detected differently based on extension)
      let files = [ (tmpDir </> "executable.exe", "This pretends to be an executable")
                   (tmpDir </> "archive.zip", "This pretends to be a zip file")
                   (tmpDir </> "image.jpg", "This pretends to be a JPG")
                  ]
                  
      -- Create the files
      mapM_ (\(path, content) -> System.IO.writeFile path content) files
      
      -- Create a config and capabilities
      let config = defaultConfig tmpDir
          readCap = fileReadCap [tmpDir]
      
      -- Test each file
      results <- mapM (\(path, _) -> 
        runClodM config $ do
          isText <- safeIsTextFile readCap path
          return (if isText then TextFile else BinaryFile)) files
      
      -- These should be detected as binary based on extension despite having text content
      let isCorrectType (Right BinaryFile) = True
          isCorrectType _ = False
      all isCorrectType results `shouldBe` True

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