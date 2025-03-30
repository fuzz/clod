{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.CapabilitySpec
-- Description : Tests for capability-based security
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the capability-based security system,
-- which is a critical component for ensuring files cannot be accessed
-- outside allowed directories.

module Clod.CapabilitySpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.Either (isRight, isLeft)
import qualified System.IO

import Clod.Types
import Clod.FileSystem.Operations (safeReadFile, safeWriteFile)
import Clod.TestHelpers (defaultTestConfig)

-- | Test specification for capability-based security
spec :: Spec
spec = do
  fileReadCapSpec
  fileWriteCapSpec
  gitCapSpec
  capabilityEscapePreventionSpec

-- | Tests for file read capabilities
fileReadCapSpec :: Spec
fileReadCapSpec = describe "File read capabilities" $ do
  it "allows access to files inside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test files
      let subDir = tmpDir </> "allowed"
      createDirectoryIfMissing True subDir
      System.IO.writeFile (subDir </> "test.txt") "test content"
      
      -- Create capability that allows access to the subDir
      let readCap = fileReadCap [subDir]
          config = defaultTestConfig tmpDir
      
      -- Test file access
      result <- runClodM config $ safeReadFile readCap (subDir </> "test.txt")
      
      -- Should succeed
      result `shouldSatisfy` isRight
      
  it "denies access to files outside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test files
      let allowedDir = tmpDir </> "allowed"
      let forbiddenDir = tmpDir </> "forbidden"
      
      createDirectoryIfMissing True allowedDir
      createDirectoryIfMissing True forbiddenDir
      
      System.IO.writeFile (allowedDir </> "allowed.txt") "allowed content"
      System.IO.writeFile (forbiddenDir </> "forbidden.txt") "forbidden content"
      
      -- Create capability that only allows access to allowedDir
      let readCap = fileReadCap [allowedDir]
          config = defaultTestConfig tmpDir
      
      -- Try to access forbidden file
      result <- runClodM config $ safeReadFile readCap (forbiddenDir </> "forbidden.txt")
      
      -- Should fail with permission error
      result `shouldSatisfy` isLeft
      case result of
        Left err -> putStrLn (show err) -- Just check that it's an error, we'll skip exact error message
        Right _ -> expectationFailure "Access was granted to a forbidden file"

-- | Tests for file write capabilities
fileWriteCapSpec :: Spec
fileWriteCapSpec = describe "File write capabilities" $ do
  it "allows writing to files inside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory
      let writeDir = tmpDir </> "writable"
      createDirectoryIfMissing True writeDir
      
      -- Create write capability
      let writeCap = fileWriteCap [writeDir]
          config = defaultTestConfig tmpDir
      
      -- Try to write to permitted directory
      result <- runClodM config $ safeWriteFile writeCap (writeDir </> "new.txt") "new content"
      
      -- Should succeed
      result `shouldSatisfy` isRight
      
      -- Verify file was created
      fileExistsCheck <- doesFileExist (writeDir </> "new.txt")
      fileExistsCheck `shouldBe` True
      
  it "denies writing to files outside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directories
      let writeDir = tmpDir </> "writable"
      let noWriteDir = tmpDir </> "not-writable"
      
      createDirectoryIfMissing True writeDir
      createDirectoryIfMissing True noWriteDir
      
      -- Create write capability that only allows certain directories
      let writeCap = fileWriteCap [writeDir]
          config = defaultTestConfig tmpDir
      
      -- Try to write to forbidden directory
      result <- runClodM config $ safeWriteFile writeCap (noWriteDir </> "forbidden.txt") "forbidden content"
      
      -- Should fail with permission error
      result `shouldSatisfy` isLeft
      case result of
        Left err -> putStrLn (show err) -- Just check that it's an error
        Right _ -> expectationFailure "Write access was granted to a forbidden directory"

-- | Tests for Checksum Database capabilities
gitCapSpec :: Spec
gitCapSpec = describe "Database capabilities" $ do
  it "allows basic file operations with capabilities" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up a test directory
      let dataDir = tmpDir </> "data-dir"
      createDirectoryIfMissing True dataDir
      
      -- Create a file
      System.IO.writeFile (dataDir </> "test-file.txt") "test content"
      
      -- Create capabilities
      let readCap = fileReadCap [dataDir]
          writeCap = fileWriteCap [dataDir]
          config = defaultTestConfig tmpDir
      
      -- Check file access
      readResult <- runClodM config $ safeReadFile readCap (dataDir </> "test-file.txt")
      
      -- Should succeed
      readResult `shouldSatisfy` isRight
      
      -- Write a new file with write capability
      writeResult <- runClodM config $ safeWriteFile writeCap (dataDir </> "new-file.txt") "new content"
      writeResult `shouldSatisfy` isRight
      
  it "restricts database operations outside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up test directories
      let allowedDir = tmpDir </> "allowed-dir"
      let forbiddenDir = tmpDir </> "forbidden-dir"
      
      -- Create the directories
      createDirectoryIfMissing True allowedDir
      createDirectoryIfMissing True forbiddenDir
      
      -- Create files in both dirs
      System.IO.writeFile (allowedDir </> "allowed.txt") "allowed content"
      System.IO.writeFile (forbiddenDir </> "forbidden.txt") "forbidden content"
      
      -- Create restricted capabilities
      let readCap = fileReadCap [allowedDir]
          writeCap = fileWriteCap [allowedDir]
          config = defaultTestConfig tmpDir
      
      -- Try to access forbidden dir
      readResult <- runClodM config $ safeReadFile readCap (forbiddenDir </> "forbidden.txt")
      
      -- Should fail with permission error
      readResult `shouldSatisfy` isLeft
      
      -- Try to write to forbidden dir
      writeResult <- runClodM config $ safeWriteFile writeCap (forbiddenDir </> "new-file.txt") "new content"
      
      -- Should fail with permission error
      writeResult `shouldSatisfy` isLeft

-- | Tests that specifically try to escape capability restrictions
capabilityEscapePreventionSpec :: Spec
capabilityEscapePreventionSpec = describe "Capability escape prevention" $ do
  it "prevents path traversal attacks with ../" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up test directories
      let allowedDir = tmpDir </> "allowed"
      let secretDir = tmpDir </> "secret"
      
      createDirectoryIfMissing True allowedDir
      createDirectoryIfMissing True secretDir
      
      -- Create a secret file
      System.IO.writeFile (secretDir </> "secret.txt") "highly sensitive data"
      
      -- Create a limited capability
      let readCap = fileReadCap [allowedDir]
          config = defaultTestConfig tmpDir
      
      -- Try path traversal attack using ../
      let traversalPath = allowedDir </> ".." </> "secret" </> "secret.txt"
      
      -- Verify the path does point to the secret file
      canonicalTraversalPath <- canonicalizePath traversalPath
      canonicalSecretPath <- canonicalizePath (secretDir </> "secret.txt")
      canonicalTraversalPath `shouldBe` canonicalSecretPath
      
      -- But capability should prevent access
      result <- runClodM config $ safeReadFile readCap traversalPath
      
      -- Should fail with permission error
      result `shouldSatisfy` isLeft
      
  it "prevents access via symbolic links outside allowed directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Skip test on Windows where symlinks might not work
      let isWindows = False -- Replace with actual OS detection if needed
      
      if isWindows
        then pendingWith "Symlink tests not applicable on Windows"
        else do
          -- Set up test directories
          let allowedDir = tmpDir </> "allowed"
          let secretDir = tmpDir </> "secret"
          
          createDirectoryIfMissing True allowedDir
          createDirectoryIfMissing True secretDir
          
          -- Create a secret file
          System.IO.writeFile (secretDir </> "secret.txt") "highly sensitive data"
          
          -- Create a symlink in the allowed directory pointing to the secret
          let linkPath = allowedDir </> "link-to-secret.txt"
          
          -- Create symlink (catching exceptions as this may fail on some systems)
          createFileLink (secretDir </> "secret.txt") linkPath
          
          -- Create a limited capability
          let readCap = fileReadCap [allowedDir]
              config = defaultTestConfig tmpDir
          
          -- Try to access via symlink
          result <- runClodM config $ safeReadFile readCap linkPath
          
          -- Should fail with permission error since the target is outside
          result `shouldSatisfy` isLeft

