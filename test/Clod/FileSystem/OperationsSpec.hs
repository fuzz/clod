{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.FileSystem.OperationsSpec
-- Description : Tests for the FileSystem.Operations module
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module tests the file system operations functionality.

module Clod.FileSystem.OperationsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory (doesFileExist, createDirectory, getCurrentDirectory,
                         withCurrentDirectory, removeFile, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.IO (Handle, hClose)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Function (on)
import Data.List (nubBy, sort)

import Clod.Types
import Clod.FileSystem.Operations

-- | Test the file system operations
spec :: Spec
spec = do
  describe "findAllFiles" $ do
    it "finds all files in a directory" $ do
      -- Create a temp directory with a nested structure
      withSystemTempDirectory "clod-test" $ \dir -> do
        -- Create directories
        createDirectory (dir </> "subdir")
        createDirectory (dir </> "subdir2")
        
        -- Create files
        BS.writeFile (dir </> "file1.txt") "test"
        BS.writeFile (dir </> "subdir" </> "file2.txt") "test"
        BS.writeFile (dir </> "subdir2" </> "file3.txt") "test"
        
        -- Run the function
        let config = ClodConfig 
              { projectPath = dir
              , stagingDir = dir
              , configDir = dir
              , lastRunFile = dir
              , timestamp = ""
              , currentStaging = dir
              , testMode = True
              , ignorePatterns = []
              }
        
        result <- runClodM config $ findAllFiles dir ["subdir", "subdir2"]
        
        -- Sort the results for deterministic comparison
        let sortedResult = sort result
        
        -- Verify results
        sortedResult `shouldBe` ["subdir/file2.txt", "subdir2/file3.txt"]

  describe "safeRemoveFile" $ do
    it "removes a file that exists" $ do
      -- Create a temp file
      withSystemTempFile "clod-test" $ \path handle -> do
        -- Close the handle so we can remove the file
        hClose handle
        
        -- Run the function
        let config = defaultTestConfig
        fileExists1 <- doesFileExist path
        fileExists1 `shouldBe` True
        
        result <- runClodM config $ safeRemoveFile path
        
        -- Verify the file doesn't exist anymore
        fileExists2 <- doesFileExist path
        fileExists2 `shouldBe` False
        
    it "doesn't error when removing a non-existent file" $ do
      -- Run the function on a non-existent file
      let config = defaultTestConfig
          path = "/this/file/does/not/exist.txt"
      
      result <- runClodM config $ safeRemoveFile path
      result `shouldBe` Right ()

  describe "safeCopyFile" $ do
    it "copies a file with proper capabilities" $ do
      -- Create a temp directory structure
      withSystemTempDirectory "clod-test" $ \dir -> do
        -- Create source file
        let src = dir </> "source.txt"
            dest = dir </> "dest.txt"
            content = "test content"
        
        BS.writeFile src content
        
        -- Create capabilities
        let readCap = fileReadCap [dir]
            writeCap = fileWriteCap [dir]
            config = defaultTestConfig
        
        -- Run the function
        result <- runClodM config $ 
          safeCopyFile readCap writeCap src dest
        
        -- Verify the file was copied
        fileExists <- doesFileExist dest
        fileExists `shouldBe` True
        
        -- Verify the content is the same
        destContent <- BS.readFile dest
        destContent `shouldBe` content
        
    it "fails when source is outside read capability" $ do
      -- Create a temp directory structure
      withSystemTempDirectory "clod-test" $ \dir -> do
        withSystemTempDirectory "clod-test-outside" $ \outsideDir -> do
          -- Create source file outside allowed directory
          let src = outsideDir </> "source.txt"
              dest = dir </> "dest.txt"
              content = "test content"
          
          BS.writeFile src content
          
          -- Create capabilities
          let readCap = fileReadCap [dir]  -- Only dir is allowed, not outsideDir
              writeCap = fileWriteCap [dir]
              config = defaultTestConfig
          
          -- Run the function
          result <- runClodM config $ 
            safeCopyFile readCap writeCap src dest
          
          -- Verify operation failed
          case result of
            Left (CapabilityError _) -> return ()
            _ -> expectationFailure "Expected CapabilityError but got different result"
          
          -- Verify the destination file doesn't exist
          fileExists <- doesFileExist dest
          fileExists `shouldBe` False

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
  , ignorePatterns = []
  }