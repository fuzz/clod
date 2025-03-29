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
import Control.Monad (forM_, void, when, forM)
import Data.Either (isRight)
import qualified Control.Exception as Exception
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

import Polysemy
import qualified Polysemy.Error as PE
import Polysemy.Reader
import Polysemy.Embed (Embed, embed)

import qualified Clod.Types as T
import Clod.Types (ClodError(..))
import qualified Clod.Effects as CE
import Clod.Capability
import Clod.FileSystem.Operations

-- | Helper function to find files in a directory with capability check
findFilesInDirectory :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                      => FileReadCap -> FilePath -> Sem r [FilePath]
findFilesInDirectory cap dir = do
  allowed <- readPolicy cap dir
  if allowed
    then do
      dirExists <- safeFileExists cap dir
      if dirExists
        then findFilesRecursive cap dir
        else PE.throw $ FileSystemError $ "Directory does not exist: " ++ dir
    else do
      canonicalPath <- embed $ canonicalizePath dir
      PE.throw $ ConfigError $ "Access denied: Cannot list files in directory outside allowed directories: " ++ canonicalPath

-- | Recursively find files in a directory with capability check
findFilesRecursive :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                   => FileReadCap -> FilePath -> Sem r [FilePath]
findFilesRecursive cap dir = do
  contents <- embed $ do
    res <- Exception.try @Exception.SomeException $ getDirectoryContents dir
    case res of
      Left err -> return []
      Right files -> return $ filter (`notElem` [".", ".."]) files
  
  files <- forM contents $ \name -> do
    let path = dir </> name
    isDir <- safeFileExists cap path >>= \exists -> 
              if exists 
                then embed $ doesDirectoryExist path 
                else return False
    if isDir
      then do
        subFiles <- findFilesRecursive cap path
        return $ map (name </>) subFiles
      else return [name]
  
  return $ concat files

-- | Copy a file with capability checks
copyFileWithCapability :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                       => FileReadCap -> FileWriteCap -> FilePath -> FilePath -> Sem r ()
copyFileWithCapability readCap writeCap src dest = do
  safeCopyFile readCap writeCap src dest

-- | Remove a file with capability check
safeRemoveFileWithCapability :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                             => FileWriteCap -> FilePath -> Sem r ()
safeRemoveFileWithCapability cap path = do
  allowed <- writePolicy cap path
  if allowed
    then do
      exists <- embed $ doesFileExist path
      when exists $ embed $ removeFile path
    else do
      canonicalPath <- embed $ canonicalizePath path
      PE.throw $ ConfigError $ "Access denied: Cannot remove file outside allowed directories: " ++ canonicalPath

-- | Run a batch of file operations with capability checks
runBatchFileOperations :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                       => FileReadCap -> FileWriteCap 
                       -> [(FilePath, FilePath)] -- ^ List of (source, destination) pairs
                       -> Sem r ()
runBatchFileOperations readCap writeCap filePairs = do
  forM_ filePairs $ \(src, dest) -> do
    safeCopyFile readCap writeCap src dest

-- | Run a batch of file operations with error handling for each operation
runBatchFileOperationsWithErrorHandling :: Members '[CE.FileSystem, PE.Error ClodError, Embed IO] r
                                       => FileReadCap -> FileWriteCap 
                                       -> [(FilePath, FilePath)] -- ^ List of (source, destination) pairs
                                       -> Sem r ()
runBatchFileOperationsWithErrorHandling readCap writeCap filePairs = do
  results <- forM filePairs $ \(src, dest) -> do
    result <- PE.catch 
      (do 
        safeCopyFile readCap writeCap src dest
        return $ Right (src, dest))
      (\e -> return $ Left (e :: ClodError))
    
    -- Add a small random delay to simulate concurrent operations
    embed $ threadDelay =<< randomRIO (1000, 5000)
    return result
    
  -- Log results
  let successes = [p | Right p <- results]
      failures = [(show err) | Left err <- results]
      
  when (not $ null failures) $ do
    CE.logInfo $ "Some file operations failed: " ++ show (length failures) ++ " out of " ++ show (length filePairs)

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
        validResult <- runM . PE.runError . CE.runFileSystemIO $ do
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
          
    it "handles non-existent source files gracefully" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up source and destination for a file that doesn't exist
        let nonExistentFile = tmpDir </> "does-not-exist.txt"
            destFile = tmpDir </> "destination.txt"
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          copyFileWithCapability readCap writeCap nonExistentFile destFile
        
        -- Should fail with a file system error
        case result of
          Left (FileSystemError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed for non-existent source file"
          
  describe "safeRemoveFile" $ do
    it "removes existing files" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create a test file
        let testFile = tmpDir </> "to-remove.txt"
        createTestFile testFile "File to be removed"
        
        -- Set up capabilities
        let writeCap = fileWriteCap [tmpDir]
        
        -- Verify file exists initially
        fileExists <- doesFileExist testFile
        fileExists `shouldBe` True
        
        -- Run the test to remove the file
        result <- runM . runError . runFileSystemIO $ do
          safeRemoveFileWithCapability writeCap testFile
        
        -- Verify the result and that the file was removed
        isRight result `shouldBe` True
        fileExistsAfter <- doesFileExist testFile
        fileExistsAfter `shouldBe` False
        
    it "handles non-existent files gracefully" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a path for a file that doesn't exist
        let nonExistentFile = tmpDir </> "does-not-exist.txt"
        
        -- Set up capabilities
        let writeCap = fileWriteCap [tmpDir]
        
        -- Run the test
        result <- runM . runError . runFileSystemIO $ do
          safeRemoveFileWithCapability writeCap nonExistentFile
        
        -- Should succeed because the operation is "safe"
        isRight result `shouldBe` True
        
    it "respects capability restrictions" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create directories with different permissions
        let allowedDir = tmpDir </> "allowed"
            restrictedDir = tmpDir </> "restricted"
            
        forM_ [allowedDir, restrictedDir] $ createDirectoryIfMissing True
        
        -- Create test files
        let allowedFile = allowedDir </> "allowed.txt"
            restrictedFile = restrictedDir </> "restricted.txt"
            
        createTestFile allowedFile "Allowed file"
        createTestFile restrictedFile "Restricted file"
        
        -- Set up capabilities that only allow access to the allowed directory
        let writeCap = fileWriteCap [allowedDir]
        
        -- Run tests
        allowedResult <- runM . runError . runFileSystemIO $ do
          safeRemoveFileWithCapability writeCap allowedFile
        
        restrictedResult <- runM . runError . runFileSystemIO $ do
          safeRemoveFileWithCapability writeCap restrictedFile
        
        -- Verify results
        isRight allowedResult `shouldBe` True
        case restrictedResult of
          Left (ConfigError _) -> return () -- Expected error
          Left err -> expectationFailure $ "Wrong error type: " ++ show err
          Right _ -> expectationFailure "Should have failed for restricted file"
          
        -- Check that only the allowed file was removed
        allowedExists <- doesFileExist allowedFile
        restrictedExists <- doesFileExist restrictedFile
        
        allowedExists `shouldBe` False
        restrictedExists `shouldBe` True
        
  describe "Concurrent operations" $ do
    it "handles multiple file operations in parallel" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test directory structure
        let srcDir = tmpDir </> "source"
            destDir = tmpDir </> "destination"
            
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True destDir
        
        -- Create 10 test files
        let numFiles = 10
            sourceFiles = [srcDir </> "file" ++ show n ++ ".txt" | n <- [1..numFiles]]
            destFiles = [destDir </> "file" ++ show n ++ ".txt" | n <- [1..numFiles]]
            
        forM_ (zip sourceFiles [1..numFiles]) $ \(file, n) ->
          createTestFile file ("Content of file " ++ show n)
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
        
        -- Run test to copy all files "in parallel"
        result <- runM . runError . runFileSystemIO $ do
          -- This uses runBatchFileOperations helper we'll need to implement
          runBatchFileOperations readCap writeCap (zip sourceFiles destFiles)
        
        -- Verify all files were copied
        isRight result `shouldBe` True
        
        forM_ (zip destFiles [1..numFiles]) $ \(file, n) -> do
          exists <- doesFileExist file
          exists `shouldBe` True
          
          content <- BS.readFile file
          map toEnum (BS.unpack content) `shouldBe` ("Content of file " ++ show n)
        
    it "handles errors gracefully in batch operations" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Create test directory structure
        let srcDir = tmpDir </> "source"
            destDir = tmpDir </> "destination"
            
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True destDir
        
        -- Create some test files, but leave some non-existent
        let numFiles = 5
            sourceFiles = [srcDir </> "file" ++ show n ++ ".txt" | n <- [1..numFiles]]
            destFiles = [destDir </> "file" ++ show n ++ ".txt" | n <- [1..numFiles]]
            
        -- Only create odd-numbered files
        forM_ (zip sourceFiles [1..numFiles]) $ \(file, n) ->
          when (odd n) $ createTestFile file ("Content of file " ++ show n)
        
        -- Set up capabilities
        let readCap = fileReadCap [tmpDir]
            writeCap = fileWriteCap [tmpDir]
        
        -- Run test to copy all files
        result <- runM . runError . runFileSystemIO $ do
          -- This uses runBatchFileOperationsWithErrorHandling helper
          runBatchFileOperationsWithErrorHandling readCap writeCap (zip sourceFiles destFiles)
        
        -- Verify the operation completed (even with some failures)
        isRight result `shouldBe` True
        
        -- Check that only the files that existed were copied
        forM_ (zip destFiles [1..numFiles]) $ \(file, n) -> do
          exists <- doesFileExist file
          if odd n
            then do
              exists `shouldBe` True
              content <- BS.readFile file
              map toEnum (BS.unpack content) `shouldBe` ("Content of file " ++ show n)
            else
              exists `shouldBe` False