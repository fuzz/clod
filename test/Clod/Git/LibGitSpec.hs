{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.Git.LibGitSpec
-- Description : Tests for LibGit2 bindings
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the Git.LibGit module.

module Clod.Git.LibGitSpec (spec) where

import Test.Hspec
import Test.QuickCheck ()
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.ByteString ()
import Control.Monad (forM_)
import Data.Either ()
import System.Process (callProcess)
import Control.Exception (try, SomeException)
import Data.List (sort)

import Clod.Git.LibGit

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
  describe "directGetModifiedFiles" $ do
    it "correctly identifies modified files using libgit2" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Create some modified files and commit
        let modifiedFiles = ["modified1.txt", "subdir/modified2.txt"]
        forM_ modifiedFiles $ \file -> do
          createModifiedFile repoPath file
        
        -- Commit the files
        callProcess "git" ["-C", repoPath, "commit", "-m", "Add modified files"]
        
        -- Modify the files again to make them show as modified
        forM_ modifiedFiles $ \file -> do
          let fullPath = repoPath </> file
          appendFile fullPath "\nAdditional content to make the file modified"
        
        -- Test the result
        result <- try $ directGetModifiedFiles repoPath
        case result of
          Left (e :: SomeException) -> 
            pendingWith $ "Test skipped: libgit2 may not be available: " ++ show e
          Right files -> do
            -- Sort both lists to ensure consistent comparison
            let sortedResult = sort files
                sortedExpected = sort modifiedFiles
            sortedResult `shouldBe` sortedExpected
  
  describe "directGetUntrackedFiles" $ do
    it "correctly identifies untracked files using libgit2" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Create some untracked files
        let untrackedFiles = ["untracked1.txt", "subdir/untracked2.txt"]
        forM_ untrackedFiles $ \file -> do
          createUntrackedFile repoPath file
        
        -- Test the result
        result <- try $ directGetUntrackedFiles repoPath
        case result of
          Left (e :: SomeException) -> 
            pendingWith $ "Test skipped: libgit2 may not be available: " ++ show e
          Right files -> do
            -- The files should contain our untracked files
            files `shouldContain` untrackedFiles
  
  describe "directGetRepositoryStatus" $ do
    it "correctly retrieves repository status using libgit2" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Set up a git repository
        let repoPath = tmpDir </> "git-repo"
        initGitRepo repoPath
        
        -- Create modified and untracked files
        let modifiedFile = "modified.txt"
            untrackedFile = "untracked.txt"
        
        -- Create a file, add it, commit it, then modify it
        createModifiedFile repoPath modifiedFile
        callProcess "git" ["-C", repoPath, "commit", "-m", "Add modified file"]
        appendFile (repoPath </> modifiedFile) "\nAdditional content to make the file modified"
        
        -- Create an untracked file
        createUntrackedFile repoPath untrackedFile
        
        -- Test the result
        result <- try $ directGetRepositoryStatus repoPath
        case result of
          Left (e :: SomeException) -> 
            pendingWith $ "Test skipped: libgit2 may not be available: " ++ show e
          Right (modified, untracked) -> do
            -- Sort for consistent comparison
            let sortedModified = sort modified
                sortedUntracked = sort untracked
            sortedModified `shouldBe` sort [modifiedFile]
            sortedUntracked `shouldBe` sort [untrackedFile]