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
import Control.Monad.IO.Class ()

import Clod.Types (ClodConfig(..), runClodM, gitCap)
import Clod.Git (safeGetModifiedFiles)
import qualified System.IO

-- | Test specification for Git operations
spec :: Spec
spec = do
  describe "Git operations with ClodM" $ do
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
            
            -- Create a Git capability and config
            let config = defaultConfig tmpDir
                gitCap' = gitCap [tmpDir]
            
            -- Run with ClodM
            result <- runClodM config $ do
              -- Use capability to access Git repository
              files <- safeGetModifiedFiles gitCap' tmpDir
              pure files
            
            -- Verify result
            result `shouldSatisfy` isRight
            
-- | Helper function to create a default config for tests
defaultConfig :: FilePath -> ClodConfig
defaultConfig tmpDir = ClodConfig
  { projectPath = tmpDir
  , stagingDir = tmpDir </> "staging"
  , configDir = tmpDir </> ".clod"
  , lastRunFile = tmpDir </> ".clod" </> "last-run"
  , timestamp = "20250401-000000"
  , currentStaging = tmpDir </> "staging"
  , testMode = True,
             verbose = False
  , ignorePatterns = []
  }
