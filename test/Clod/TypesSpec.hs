{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clod.TypesSpec
-- Description : Tests for core types
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the core types of the Clod application.

module Clod.TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Success)
import Control.Exception ()
import Data.Text ()
import System.FilePath
import System.IO.Temp ()
import Control.Monad.Reader ()
import Control.Monad.Except ()

import Clod.Types

-- | Property: OptimizedName should preserve its structure through the newtype
prop_optimizedNameRoundTrip :: String -> Bool
prop_optimizedNameRoundTrip s = unOptimizedName (OptimizedName s) == s

-- | Property: OriginalPath should preserve its structure through the newtype
prop_originalPathRoundTrip :: FilePath -> Bool 
prop_originalPathRoundTrip p = unOriginalPath (OriginalPath p) == p

spec :: Spec
spec = do
  describe "Newtypes and smart constructors" $ do
    it "OptimizedName preserves the original string" $ do
      property prop_optimizedNameRoundTrip
      
    it "OriginalPath preserves the original path" $ do
      property prop_originalPathRoundTrip
      
    it "can create an optimized name" $ do
      let name = OptimizedName "test.txt"
      unOptimizedName name `shouldBe` "test.txt"
      
    it "can create an original path" $ do
      let path = OriginalPath "/path/to/file.txt"
      unOriginalPath path `shouldBe` "/path/to/file.txt"
  
  describe "ClodError" $ do
    it "can be created and displayed" $ do
      show (ConfigError "test error") `shouldBe` "ConfigError \"test error\""
      show (GitError "git error") `shouldBe` "GitError \"git error\""
      show (FileSystemError "file.txt" (userError "fs error")) `shouldBe` "FileSystemError \"file.txt\" user error (fs error)"
  
  describe "FileResult" $ do
    it "can be created and displayed" $ do
      show (Success) `shouldBe` "Success"
      show (Skipped "reason") `shouldBe` "Skipped \"reason\""
  
  describe "ClodM Monad Stack" $ do
    it "handles reader operations correctly" $ do
      let mtlComputation = do
            config <- ask
            return (projectPath config) :: ClodM String
            
      mtlResult <- runExceptT $ runReaderT mtlComputation (defaultConfig "test-dir")
      mtlResult `shouldBe` Right "test-dir"
      
    it "handles errors correctly" $ do
      let mtlError = throwError (ConfigError "test error") :: ClodM String
          
      mtlResult <- runExceptT $ runReaderT mtlError (defaultConfig "test")
      mtlResult `shouldBe` Left (ConfigError "test error")
  
  where
    defaultConfig dir = ClodConfig 
      { projectPath = dir
      , stagingDir = dir </> "staging"
      , configDir = dir </> "config"
      , lastRunFile = dir </> ".clod-last-run"
      , timestamp = "20250401-120000"
      , currentStaging = dir </> "staging"
      , testMode = True
      , ignorePatterns = []
      }