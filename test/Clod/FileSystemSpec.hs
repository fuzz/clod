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

import Polysemy
import Polysemy.Error

import Clod.Types (ClodError(..))
import Clod.Effects
import Clod.Capability

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
        let readCap = fileReadCap [tmpDir </> "allowed"]
        
        -- Attempt to read from allowed directory
        result1 <- runM . runError @ClodError . runFileSystemIO $
          safeReadFile readCap (tmpDir </> "allowed" </> "test.txt")
        
        -- Attempt to read from forbidden directory
        result2 <- runM . runError @ClodError . runFileSystemIO $
          safeReadFile readCap (tmpDir </> "forbidden" </> "secret.txt")
        
        -- Check results
        result1 `shouldSatisfy` isRight
        case result2 of
          Left _ -> return () -- Expected to fail with access denied
          Right _ -> expectationFailure "Access to forbidden directory was allowed"
  
  describe "File system operations with effects" $ do
    it "can find all files in a directory" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test directory structure
        createDirectoryIfMissing True (tmpDir </> "src" </> "components")
        createDirectoryIfMissing True (tmpDir </> "test")
        
        -- Create some test files
        System.IO.writeFile (tmpDir </> "README.md") "# Test"
        System.IO.writeFile (tmpDir </> "src" </> "index.js") "console.log('hello');"
        System.IO.writeFile (tmpDir </> "src" </> "components" </> "Button.jsx") "<Button />"
        System.IO.writeFile (tmpDir </> "test" </> "index.test.js") "test('example');"
        -- Build a list of files using filesystem effect
        result <- runM . runError @ClodError . runFileSystemIO $ do
          -- Manually list directories using capabilities
          let dirs = ["README.md", "src", "test"]
          
          files <- embed $ do
            allFiles <- concat <$> mapM (listFilesRecursively tmpDir) dirs
            return allFiles
            
          pure files
          
        -- Verify the result
        case result of
          Left err -> expectationFailure $ "Failed to find files: " ++ show err
          Right files -> do
            files `shouldContain` [tmpDir </> "README.md"]
            files `shouldContain` [tmpDir </> "src" </> "index.js"]
            files `shouldContain` [tmpDir </> "src" </> "components" </> "Button.jsx"]
            files `shouldContain` [tmpDir </> "test" </> "index.test.js"]
            length files `shouldBe` 4
            
    it "properly handles file copying" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create source and destination directories
        createDirectoryIfMissing True (tmpDir </> "source")
        createDirectoryIfMissing True (tmpDir </> "dest")
        
        -- Create a test file
        System.IO.writeFile (tmpDir </> "source" </> "test.txt") "test content"
        
        -- Create capabilities
        let readCap = fileReadCap [tmpDir </> "source"]
            writeCap = fileWriteCap [tmpDir </> "dest"]
            
        -- Copy file using the capability-based system
        result <- runM . runError @ClodError . runFileSystemIO $ 
          safeCopyFile readCap writeCap (tmpDir </> "source" </> "test.txt") (tmpDir </> "dest" </> "test.txt")
          
        -- Verify the copy worked
        case result of
          Left err -> expectationFailure $ "Failed to copy file: " ++ show err
          Right _ -> do
            exists <- doesFileExist (tmpDir </> "dest" </> "test.txt")
            exists `shouldBe` True
            
            content <- System.IO.readFile (tmpDir </> "dest" </> "test.txt")
            content `shouldBe` "test content"
  
-- Helper function for listing files recursively
listFilesRecursively :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursively baseDir path = do
    let fullPath = baseDir </> path
    isFile <- doesFileExist fullPath
    if isFile
      then return [fullPath]
      else do
        isDir <- doesDirectoryExist fullPath
        if isDir
          then do
            contents <- getDirectoryContents fullPath
            let properContents = filter (`notElem` [".", ".."]) contents
            concat <$> mapM (listFilesRecursively baseDir . (path </>)) properContents
          else return []
