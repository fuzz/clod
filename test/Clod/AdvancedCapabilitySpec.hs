{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.AdvancedCapabilitySpec
-- Description : Tests for the advanced capability system
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the advanced capability-based security system
-- that uses type-level programming to enforce security constraints.

module Clod.AdvancedCapabilitySpec (spec) where

import Test.Hspec
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

import Clod.AdvancedCapability hiding (readFile, writeFile)
import qualified Clod.AdvancedCapability as AC

-- | Test specification for AdvancedCapability module
spec :: Spec
spec = do
  describe "Advanced capability system" $ do
    typeRestrictedFileAccessSpec
    pathTraversalProtectionSpec
    permissionConstraintsSpec
    
-- | Tests for type-restricted file access
typeRestrictedFileAccessSpec :: Spec
typeRestrictedFileAccessSpec = describe "Type-restricted file access" $ do
  it "allows access to files inside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory and file
      let testDir = tmpDir </> "test-dir"
      let testFile = testDir </> "test-file.txt"
      createDirectoryIfMissing True testDir
      BS.writeFile testFile "Hello, World!"
      
      -- Create capability for the test directory
      let cap = createCapability @'Read [testDir]
      
      -- Use withPath to create a typed path
      withPath cap testFile $ \pathMaybe -> do
        -- pathMaybe should be Just because the path is allowed
        case pathMaybe of
          Nothing -> expectationFailure "Path should be allowed but wasn't"
          Just typedPath -> do
            -- Read file with the capability
            content <- AC.readFile cap typedPath
            content `shouldBe` "Hello, World!"
            
  it "denies access to files outside permitted directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create allowed and forbidden directories
      let allowedDir = tmpDir </> "allowed"
      let forbiddenDir = tmpDir </> "forbidden"
      createDirectoryIfMissing True allowedDir
      createDirectoryIfMissing True forbiddenDir
      
      -- Create files in both directories
      let allowedFile = allowedDir </> "allowed.txt"
      let forbiddenFile = forbiddenDir </> "forbidden.txt"
      BS.writeFile allowedFile "Allowed content"
      BS.writeFile forbiddenFile "Forbidden content"
      
      -- Create capability for the allowed directory only
      let cap = createCapability @'Read [allowedDir]
      
      -- Try to access forbidden file
      withPath cap forbiddenFile $ \pathMaybe -> do
        -- pathMaybe should be Nothing because the path is not allowed
        pathMaybe `shouldBe` Nothing

-- | Tests for path traversal protection
pathTraversalProtectionSpec :: Spec
pathTraversalProtectionSpec = describe "Path traversal protection" $ do
  it "prevents path traversal attacks with ../" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory structure
      let safeDir = tmpDir </> "safe"
      let dataFile = safeDir </> "data.txt"
      let secretDir = tmpDir </> "secret"
      let secretFile = secretDir </> "secret.txt"
      
      createDirectoryIfMissing True safeDir
      createDirectoryIfMissing True secretDir
      BS.writeFile dataFile "Safe data"
      BS.writeFile secretFile "Secret data"
      
      -- Create capability for the safe directory only
      let cap = createCapability @'Read [safeDir]
      
      -- Try to access secret file through path traversal
      let traversalPath = safeDir </> "../secret/secret.txt"
      
      withPath cap traversalPath $ \pathMaybe -> do
        -- pathMaybe should be Nothing because path traversal is prevented
        pathMaybe `shouldBe` Nothing

-- | Tests for permission constraints
permissionConstraintsSpec :: Spec
permissionConstraintsSpec = describe "Permission constraints" $ do
  it "allows write operations with write capability" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory
      let testDir = tmpDir </> "write-test"
      let testFile = testDir </> "writeable.txt"
      createDirectoryIfMissing True testDir
      
      -- Create write capability
      let writeCap = createCapability @'Write [testDir]
      
      -- Use withPath to get a typed path
      withPath writeCap testFile $ \pathMaybe -> do
        case pathMaybe of
          Nothing -> expectationFailure "Path should be allowed for writing"
          Just typedPath -> do
            -- Write to the file
            AC.writeFile writeCap typedPath "Written with capability"
            
            -- Verify content was written
            fileExists <- liftIO $ doesFileExist testFile
            fileExists `shouldBe` True
            content <- liftIO $ BS.readFile testFile
            content `shouldBe` "Written with capability"
            
  it "allows using AllPerm for both read and write" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory
      let testDir = tmpDir </> "all-perm-test"
      let testFile = testDir </> "all-access.txt"
      createDirectoryIfMissing True testDir
      
      -- Create all-permission capability
      let allCap = createCapability @'All [testDir]
      
      -- Use withPath to get a typed path
      withPath allCap testFile $ \pathMaybe -> do
        case pathMaybe of
          Nothing -> expectationFailure "Path should be allowed with AllPerm"
          Just typedPath -> do
            -- Write to file
            AC.writeFile allCap typedPath "Initial content"
            
            -- Read from file
            content <- AC.readFile allCap typedPath
            content `shouldBe` "Initial content"
  
  it "allows capability restriction to more limited permissions" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Create test directory
      let testDir = tmpDir </> "restriction-test"
      let testFile = testDir </> "restrict.txt"
      createDirectoryIfMissing True testDir
      BS.writeFile testFile "Original content"
      
      -- Create all-permission capability
      let allCap = createCapability @'All [testDir]
      
      -- Restrict to read-only
      let readCap = restrictCapability allCap
      
      -- Use withPath to get a typed path
      withPath readCap testFile $ \pathMaybe -> do
        case pathMaybe of
          Nothing -> expectationFailure "Path should be allowed for reading"
          Just typedPath -> do
            -- Read should be allowed
            content <- AC.readFile readCap typedPath
            content `shouldBe` "Original content"
            
            -- This would fail to compile:
            -- writeFile readCap typedPath "New content" -- Type error!