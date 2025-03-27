{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.GitSpec
-- Description : Tests for Git-related operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for Git-related operations.

module Clod.GitSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Control.Exception (try, SomeException)

import Clod.Git
import Clod.Types

-- | Test specification for Git operations
spec :: Spec
spec = do
  describe "getRepositoryRoot" $ do
    it "finds the git repository root correctly" $ do
      -- This test depends on the test being run from within a git repository
      -- or from a location where git can find the repository
      rootEither <- runClodM getRepositoryRoot
      case rootEither of
        Left err -> expectationFailure $ "Failed to find git root: " ++ show err
        Right root -> do
          -- Check if the path exists and has a .git directory
          doesDirectoryExist (root </> ".git") >>= \exists ->
            exists `shouldBe` True
  
  describe "getGitNewFiles" $ do
    it "identifies untracked files correctly" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Initialize a git repository
        initResult <- try $ callProcess "git" ["-C", tmpDir, "init"] :: IO (Either SomeException ())
        case initResult of
          Left _ -> pendingWith "Skip test: Unable to initialize git repository"
          Right _ -> do
            -- Set git config
            _ <- try $ callProcess "git" ["-C", tmpDir, "config", "user.email", "test@example.com"] :: IO (Either SomeException ())
            _ <- try $ callProcess "git" ["-C", tmpDir, "config", "user.name", "Test User"] :: IO (Either SomeException ())
            
            -- Create and track a file
            writeFile (tmpDir </> "tracked.txt") "tracked content"
            _ <- try $ callProcess "git" ["-C", tmpDir, "add", "tracked.txt"] :: IO (Either SomeException ())
            _ <- try $ callProcess "git" ["-C", tmpDir, "commit", "-m", "Add tracked file"] :: IO (Either SomeException ())
            
            -- Create an untracked file
            writeFile (tmpDir </> "untracked.txt") "untracked content"
            
            -- Get untracked files via ClodM monad
            untrackedEither <- runClodM $ getGitNewFiles tmpDir
            case untrackedEither of
              Left err -> expectationFailure $ "Failed to get untracked files: " ++ show err
              Right untracked -> untracked `shouldContain` ["untracked.txt"]