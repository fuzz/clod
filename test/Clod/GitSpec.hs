{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import qualified Control.Exception as Exception
import Control.Exception (SomeException)
import Data.Either (isRight)

import Polysemy
import Polysemy.Error

import qualified Clod.Types as T
import Clod.Effects
import qualified Clod.Capability as Cap
import qualified System.IO

-- | Test specification for Git operations
spec :: Spec
spec = do
  describe "Git operations with the effects system" $ do
    it "can restrict Git operations to specific repositories" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        -- Initialize a git repository
        initResult <- Exception.try $ callProcess "git" ["-C", tmpDir, "init"] :: IO (Either SomeException ())
        case initResult of
          Left _ -> pendingWith "Skip test: Unable to initialize git repository"
          Right _ -> do
            -- Set git config
            _ <- Exception.try $ callProcess "git" ["-C", tmpDir, "config", "user.email", "test@example.com"] :: IO (Either SomeException ())
            _ <- Exception.try $ callProcess "git" ["-C", tmpDir, "config", "user.name", "Test User"] :: IO (Either SomeException ())
            
            -- Create and track a file
            System.IO.writeFile (tmpDir </> "tracked.txt") "tracked content"
            _ <- Exception.try $ callProcess "git" ["-C", tmpDir, "add", "tracked.txt"] :: IO (Either SomeException ())
            _ <- Exception.try $ callProcess "git" ["-C", tmpDir, "commit", "-m", "Add tracked file"] :: IO (Either SomeException ())
            
            -- Create an untracked file
            System.IO.writeFile (tmpDir </> "untracked.txt") "untracked content"
            
            -- Create a Git capability that allows access to the repo
            let repoCapability = Cap.gitCap [tmpDir]
            
            -- Run with effects
            result <- runM . runError @T.ClodError . runGitIO $ do
              -- Use capability to access Git repository
              files <- Cap.safeGetModifiedFiles repoCapability tmpDir
              pure files
            
            -- Verify result
            result `shouldSatisfy` isRight