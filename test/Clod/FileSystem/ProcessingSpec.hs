{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.ProcessingSpec
-- Description : Tests for the FileSystem.Processing module
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module tests the file processing functionality, including
-- manifest generation and optimization.

module Clod.FileSystem.ProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck ()
import System.Directory (doesFileExist, createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception ()
import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Reader ()
import qualified Data.ByteString as BS
import Data.Either ()
import Data.Function ()
import Data.List ()

import Clod.Types
import Clod.FileSystem.Processing

-- | Test the file processing functionality
spec :: Spec
spec = do
  describe "createOptimizedName" $ do
    it "converts directory separators to dashes" $ do
      unOptimizedName (createOptimizedName "src/main.js") `shouldBe` "src-main.js"
      unOptimizedName (createOptimizedName "nested/dirs/file.txt") `shouldBe` "nested-dirs-file.txt"
      
    it "leaves files without directories unchanged" $ do
      unOptimizedName (createOptimizedName "file.txt") `shouldBe` "file.txt"
      
    it "handles hidden files correctly" $ do
      unOptimizedName (createOptimizedName ".gitignore") `shouldBe` "dot--gitignore"
      unOptimizedName (createOptimizedName "src/.env") `shouldBe` "src-dot--env"
      unOptimizedName (createOptimizedName ".config/settings.json") `shouldBe` "dot--config-settings.json"
      
    it "handles complex paths with multiple hidden components" $ do
      unOptimizedName (createOptimizedName ".hidden/.nested/.config/file.txt") `shouldBe` "dot--hidden-dot--nested-dot--config-file.txt"
      unOptimizedName (createOptimizedName "projects/.git/config") `shouldBe` "projects-dot--git-config"
      unOptimizedName (createOptimizedName ".vscode/launch.json") `shouldBe` "dot--vscode-launch.json"

  describe "escapeJSON" $ do
    it "escapes backslashes and quotes" $ do
      escapeJSON "path/with/\"quotes\"" `shouldBe` "path/with/\\\"quotes\\\""
      escapeJSON "Windows\\style\\path" `shouldBe` "Windows\\\\style\\\\path"
      escapeJSON "Combined \"quotes\" and \\backslashes\\" `shouldBe` "Combined \\\"quotes\\\" and \\\\backslashes\\\\"
      
    it "leaves other characters unchanged" $ do
      escapeJSON "normal text" `shouldBe` "normal text"
      escapeJSON "symbols" `shouldBe` "symbols"

  describe "writeManifestFile" $ do
    it "writes entries to the manifest file" $ do
      -- Create a temp directory
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a manifest file path and entries
        let manifestPath = tmpDir </> "manifest.json"
            entry1 = ManifestEntry 
              { entryOptimizedName = OptimizedName "src-main.js"
              , entryOriginalPath = OriginalPath "src/main.js"
              }
            entry2 = ManifestEntry
              { entryOptimizedName = OptimizedName "src-utils.js"
              , entryOriginalPath = OriginalPath "src/utils.js"
              }
            entries = [entry1, entry2]
            config = defaultTestConfig
        
        -- Run the function
        result <- runClodM config $ writeManifestFile manifestPath entries
        result `shouldBe` Right ()
        
        -- Check the manifest file was created with both entries
        manifestExists <- doesFileExist manifestPath
        manifestExists `shouldBe` True
        
        -- Check the content
        manifestContent <- readFile manifestPath
        -- Verify both entries exist properly
        manifestContent `shouldContain` "\"src-main.js\": \"src/main.js\""
        manifestContent `shouldContain` "\"src-utils.js\": \"src/utils.js\""
        -- And proper JSON structure
        manifestContent `shouldContain` "{\n"
        manifestContent `shouldContain` "\n}"
        
  describe "processFiles" $ do
    it "processes a list of files and creates a manifest" $ do
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            manifestPath = stagingDir </> "_path_manifest.json"
            newFilePath = sourceDir </> "newfile.txt"
            relPath = "newfile.txt"
            content = "This is a new file"
            
        createDirectory sourceDir
        createDirectory stagingDir
        
        -- Create a new file
        BS.writeFile newFilePath (BS.pack $ map (toEnum . fromEnum) content)
        
        -- Create config
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , verbose = False
              , ignorePatterns = []
              }
        
        -- Run processFiles with the new file
        result <- runClodM config $ processFiles config manifestPath [relPath] False
        
        -- Check it succeeded
        case result of
          Left err -> expectationFailure $ "Error processing files: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 1
            skipped `shouldBe` 0
        
        -- Verify the manifest was created
        manifestExists <- doesFileExist manifestPath
        manifestExists `shouldBe` True
        
        -- Check that the file was copied to staging
        let stagingFile = stagingDir </> relPath
        fileExists <- doesFileExist stagingFile
        fileExists `shouldBe` True
        
        -- Check the content of the copied file
        copiedContent <- BS.unpack <$> BS.readFile stagingFile
        map (toEnum . fromEnum) copiedContent `shouldBe` content
        
        -- Check the manifest content
        manifestContent <- readFile manifestPath
        manifestContent `shouldContain` "\"newfile.txt\": \"newfile.txt\""
          
    it "creates a manifest even with no files" $ do
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            manifestPath = stagingDir </> "_path_manifest.json"
            
        createDirectory sourceDir
        createDirectory stagingDir
        
        -- Create config
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , verbose = False
              , ignorePatterns = []
              }
        
        -- Run processFiles with an empty list
        result <- runClodM config $ processFiles config manifestPath [] False
        
        -- Check it succeeded
        case result of
          Left err -> expectationFailure $ "Error processing files: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 0
            skipped `shouldBe` 0
        
        -- Verify the manifest was created
        manifestExists <- doesFileExist manifestPath
        manifestExists `shouldBe` True
        
        -- Check the manifest content - should be an empty JSON object
        manifestContent <- readFile manifestPath
        manifestContent `shouldContain` "{\n"
        manifestContent `shouldContain` "\n}"
        
    it "properly processes multiple files in one run" $ do
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            manifestPath = stagingDir </> "_path_manifest.json"
            file1Path = sourceDir </> "file1.txt"
            file2Path = sourceDir </> "file2.txt"
            relPath1 = "file1.txt"
            relPath2 = "file2.txt"
            content1 = "This is file 1"
            content2 = "This is file 2"
            
        createDirectory sourceDir
        createDirectory stagingDir
        
        -- Create two files
        BS.writeFile file1Path (BS.pack $ map (toEnum . fromEnum) content1)
        BS.writeFile file2Path (BS.pack $ map (toEnum . fromEnum) content2)
        
        -- Create config
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , verbose = False
              , ignorePatterns = []
              }
        
        -- Run processFiles with both files
        result <- runClodM config $ processFiles config manifestPath [relPath1, relPath2] False
        
        -- Check it succeeded
        case result of
          Left err -> expectationFailure $ "Error processing files: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 2
            skipped `shouldBe` 0
        
        -- Verify both files were copied to staging
        let stagingFile1 = stagingDir </> relPath1
            stagingFile2 = stagingDir </> relPath2
        
        file1Exists <- doesFileExist stagingFile1
        file1Exists `shouldBe` True
        
        file2Exists <- doesFileExist stagingFile2
        file2Exists `shouldBe` True
        
        -- Check the content of the copied files
        copiedContent1 <- BS.unpack <$> BS.readFile stagingFile1
        map (toEnum . fromEnum) copiedContent1 `shouldBe` content1
        
        copiedContent2 <- BS.unpack <$> BS.readFile stagingFile2
        map (toEnum . fromEnum) copiedContent2 `shouldBe` content2
        
        -- Check the manifest content
        manifestContent <- readFile manifestPath
        manifestContent `shouldContain` "\"file1.txt\": \"file1.txt\""
        manifestContent `shouldContain` "\"file2.txt\": \"file2.txt\""
        
    it "manifests both staged and not-staged files correctly" $ do
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            manifestPath = stagingDir </> "_path_manifest.json"
            file1Path = sourceDir </> "file1.txt"
            file2Path = sourceDir </> "file2.txt"
            relPath1 = "file1.txt"
            relPath2 = "file2.txt"
            content1 = "This is file 1"
            content2 = "This is file 2"
            
        createDirectory sourceDir
        createDirectory stagingDir
        
        -- Create two files
        BS.writeFile file1Path (BS.pack $ map (toEnum . fromEnum) content1)
        BS.writeFile file2Path (BS.pack $ map (toEnum . fromEnum) content2)
        
        -- Create config
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , verbose = False
              , ignorePatterns = []
              }
        
        -- First add all files to manifest without copying
        result1 <- runClodM config $ processFiles config manifestPath [relPath1, relPath2] True
        
        -- Check it succeeded
        case result1 of
          Left err -> expectationFailure $ "Error creating manifest: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 2
            skipped `shouldBe` 0
        
        -- Now copy just one file to staging
        result2 <- runClodM config $ processFiles config manifestPath [relPath1] False
        
        -- Check it succeeded
        case result2 of
          Left err -> expectationFailure $ "Error processing file: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 1
            skipped `shouldBe` 0
        
        -- Verify only file1 was copied to staging
        let stagingFile1 = stagingDir </> relPath1
            stagingFile2 = stagingDir </> relPath2
        
        file1Exists <- doesFileExist stagingFile1
        file1Exists `shouldBe` True
        
        file2Exists <- doesFileExist stagingFile2
        file2Exists `shouldBe` False
        
        -- Check the manifest content - should contain both files
        manifestContent <- readFile manifestPath
        manifestContent `shouldContain` "\"file1.txt\": \"file1.txt\""
        manifestContent `shouldContain` "\"file2.txt\": \"file2.txt\""
        
    it "skips binary files" $ do
      -- Create source and staging directories
      withSystemTempDirectory "clod-test" $ \dir -> do
        let sourceDir = dir </> "source"
            stagingDir = dir </> "staging"
            binaryPath = sourceDir </> "binary.bin"
            textPath = sourceDir </> "text.txt"
            relBinaryPath = "binary.bin"
            relTextPath = "text.txt"
            manifestPath = stagingDir </> "manifest.json"
            textContent = "This is a text file"
            
        -- Create binary and text content
        createDirectory sourceDir
        createDirectory stagingDir
        BS.writeFile binaryPath $ BS.pack [0x00, 0x01, 0x02, 0x03, 0x7F, 0xFF, 0x4D, 0x5A]
        BS.writeFile textPath (BS.pack $ map (toEnum . fromEnum) textContent)
        
        -- Create config
        let config = ClodConfig 
              { projectPath = sourceDir
              , stagingDir = stagingDir
              , configDir = dir
              , lastRunFile = dir </> "lastrun"
              , timestamp = ""
              , currentStaging = stagingDir
              , testMode = True
              , verbose = False
              , ignorePatterns = []
              }
            
        -- Run processFiles with both files
        result <- runClodM config $ processFiles config manifestPath [relBinaryPath, relTextPath] False
        
        -- Check result - should have processed 1 file (text) and skipped 1 (binary)
        case result of
          Left err -> expectationFailure $ "Error processing files: " ++ show err
          Right (processed, skipped) -> do
            processed `shouldBe` 1
            skipped `shouldBe` 1
        
        -- Verify only the text file was copied to staging
        let stagingTextFile = stagingDir </> relTextPath
            stagingBinaryFile = stagingDir </> relBinaryPath
        
        textFileExists <- doesFileExist stagingTextFile
        textFileExists `shouldBe` True
        
        binaryFileExists <- doesFileExist stagingBinaryFile
        binaryFileExists `shouldBe` False
        
        -- Check the manifest content - should contain only the text file
        manifestContent <- readFile manifestPath
        manifestContent `shouldContain` "\"text.txt\": \"text.txt\""
        manifestContent `shouldNotContain` "\"binary.bin\": \"binary.bin\""

-- | Default test configuration
defaultTestConfig :: ClodConfig
defaultTestConfig = ClodConfig
  { projectPath = "/"
  , stagingDir = "/"
  , configDir = "/"
  , lastRunFile = "/"
  , timestamp = ""
  , currentStaging = "/"
  , testMode = True
  , verbose = False
  , ignorePatterns = []
  }
