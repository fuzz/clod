{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.Git.InternalSpec
-- Description : Tests for Git internal operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the Git.Internal module.

module Clod.Git.InternalSpec (spec) where

import Test.Hspec
import Test.QuickCheck ()
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.ByteString ()
import Control.Monad (forM_)
import Data.Either ()
import System.Process (callProcess)
import Data.List (sort)

import Clod.Git.Internal

-- | Initialize a git repository
initGitRepo :: FilePath -> IO ()
initGitRepo path = do
  createDirectoryIfMissing True path
  callProcess "git" ["-C", path, "init"]
  -- Create and commit a sample file
  let sampleFile = path </> "sample.txt"
  writeFile sampleFile "Sample content for git repository"
  callProcess "git" ["-C", path, "add", "sample.txt"]
  callProcess "git" ["-C", path, "config", "user.name", "Test User"]
  callProcess "git" ["-C", path, "config", "user.email", "test@example.com"]
  callProcess "git" ["-C", path, "commit", "-m", "Initial commit"]

-- | Create a modified file in a git repository
createModifiedFile :: FilePath -> String -> IO ()
createModifiedFile repoPath filePath = do
  let fullPath = repoPath </> filePath
  createDirectoryIfMissing True (takeDirectory fullPath)
  writeFile fullPath ("Modified content for " ++ filePath)
  callProcess "git" ["-C", repoPath, "add", filePath]

-- | Create an untracked file in a git repository
createUntrackedFile :: FilePath -> String -> IO ()
createUntrackedFile repoPath filePath = do
  let fullPath = repoPath </> filePath
  createDirectoryIfMissing True (takeDirectory fullPath)
  writeFile fullPath ("Untracked content for " ++ filePath)

spec :: Spec
spec = do
  describe "isGitRepo" $ do
    it "correctly identifies Git repositories" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Set up a non-git directory
        let nonRepoPath = tmpDir </> "non-git-dir"
        createDirectoryIfMissing True nonRepoPath
        
        -- Test the results
        isGitRepo repoPath `shouldReturn` True
        isGitRepo nonRepoPath `shouldReturn` False
        isGitRepo (tmpDir </> "nonexistent") `shouldReturn` False
  
  describe "getRepoRootPath" $ do
    it "returns the root path of a git repository" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository with nested directories
        let repoPath = tmpDir </> "git-repo"
            nestedPath = repoPath </> "nested" </> "dir"
        
        initGitRepo repoPath
        createDirectoryIfMissing True nestedPath
        
        -- Test with repository root path
        rootResult1 <- getRepoRootPath repoPath
        rootResult1 `shouldBe` Just repoPath
        
        -- Test with nested directory path
        rootResult2 <- getRepoRootPath nestedPath
        rootResult2 `shouldBe` Just repoPath
        
        -- Test with non-git directory
        nonGitResult <- getRepoRootPath (tmpDir </> "non-git-dir")
        nonGitResult `shouldBe` Nothing
  
  describe "listModifiedFiles" $ do
    it "correctly lists modified files in a git repository" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository with modified files
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Create some modified files
        let modifiedFiles = ["modified1.txt", "modified2.md", "subdir/modified3.txt"]
        forM_ modifiedFiles $ \file -> do
          createModifiedFile repoPath file
        
        -- Test the result
        modifiedResult <- listModifiedFiles repoPath
        length modifiedResult `shouldBe` length modifiedFiles
        modifiedResult `shouldContain` modifiedFiles
  
  describe "listUntrackedFiles" $ do
    it "correctly lists untracked files in a git repository" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository with untracked files
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Create some untracked files
        let untrackedFiles = ["untracked1.txt", "untracked2.md", "subdir/untracked3.txt"]
        forM_ untrackedFiles $ \file -> do
          createUntrackedFile repoPath file
        
        -- Test the result
        untrackedResult <- listUntrackedFiles repoPath
        length untrackedResult `shouldBe` length untrackedFiles
        -- Sort both lists to ensure consistent comparison
        let sortedResult = sort untrackedResult
            sortedExpected = sort untrackedFiles
        sortedResult `shouldBe` sortedExpected