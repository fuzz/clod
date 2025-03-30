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
import Test.QuickCheck ()
import System.Directory (doesFileExist, createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.IO (hClose)
import Control.Exception ()
import Control.Monad ()
import Control.Monad.Except ()
import Control.Monad.Reader ()
import qualified Data.ByteString as BS
import Data.Either ()
import Data.Function ()
import Data.List (sort)

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
               stagingDir = dir
               configDir = dir
               databaseFile = tmpDir </> ".clod" </> "database.dhall",
  previousStaging = Nothing,
  flushMode = False,
  lastMode = False,
               timestamp = ""
               currentStaging = dir
               testMode = True
               verbose = False
               ignorePatterns = []
              }
        
        result <- runClodM config $ findAllFiles dir ["subdir", "subdir2"]
        
        -- Verify results with sorted output for deterministic comparison
        case result of
          Left err -> expectationFailure $ "Error finding files: " ++ show err
          Right files -> sort files `shouldBe` ["subdir/file2.txt", "subdir2/file3.txt"]
          
    it "avoids './' prefix when finding files in root directory" $ do
      -- Create a temp directory with files in the root
      withSystemTempDirectory "clod-test" $ \dir -> do
        -- Create files directly in root directory
        BS.writeFile (dir </> "root1.txt") "test"
        BS.writeFile (dir </> "root2.txt") "test"
        
        -- Run the function with empty string (new approach for root dir)
        let config = ClodConfig 
              { projectPath = dir
               stagingDir = dir
               configDir = dir
               databaseFile = tmpDir </> ".clod" </> "database.dhall",
  previousStaging = Nothing,
  flushMode = False,
  lastMode = False,
               timestamp = ""
               currentStaging = dir
               testMode = True
               verbose = False
               ignorePatterns = []
              }
        
        result <- runClodM config $ findAllFiles dir [""]
        
        -- Verify results don't have "./" prefix
        case result of
          Left err -> expectationFailure $ "Error finding files: " ++ show err
          Right files -> do
            sort files `shouldBe` ["root1.txt", "root2.txt"]
            
            -- Explicitly verify no file has "./" prefix
            all (\f -> not (take 2 f == "./")) files `shouldBe` True

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
        
        _ <- runClodM config $ safeRemoveFile path
        
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
        _ <- runClodM config $ 
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
   stagingDir = "/"
   configDir = "/"
   databaseFile = tmpDir </> ".clod" </> "database.dhall",
  previousStaging = Nothing,
  flushMode = False,
  lastMode = False,
   timestamp = ""
   currentStaging = "/"
   testMode = True
   verbose = False
   ignorePatterns = []
  }