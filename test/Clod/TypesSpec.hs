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
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Text as T
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad.Reader
import Control.Monad.Except
import Polysemy
import Polysemy.Reader
import Polysemy.Error

import Clod.Types

-- | Property: OptimizedName should preserve its structure through the newtype
prop_optimizedNameRoundTrip :: String -> Bool
prop_optimizedNameRoundTrip s = unOptimizedName (OptimizedName s) == s

-- | Property: OriginalPath should preserve its structure through the newtype
prop_originalPathRoundTrip :: FilePath -> Bool 
prop_originalPathRoundTrip p = getOriginalPath (OriginalPath p) == p

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
      getOriginalPath path `shouldBe` "/path/to/file.txt"
  
  describe "ClodError" $ do
    it "can be created and displayed" $ do
      show (ConfigError "test error") `shouldBe` "ConfigError \"test error\""
      show (GitError "git error") `shouldBe` "GitError \"git error\""
      show (FileSystemError "fs error") `shouldBe` "FileSystemError \"fs error\""
  
  describe "FileResult" $ do
    it "can be created and displayed" $ do
      show Success `shouldBe` "Success"
      show (Skipped "reason") `shouldBe` "Skipped \"reason\""
  
  describe "Effects vs Monad Transformers" $ do
    it "transforms ClodM to the effects system" $ do
      let mtlComputation = do
            config <- ask
            return (projectDir config) :: ClodM String
            
          effectsComputation = do
            config <- ask @ClodConfig
            return (projectDir config) :: Sem '[Reader ClodConfig] String
            
      mtlResult <- runExceptT $ runReaderT mtlComputation (defaultConfig "test-dir")
      mtlResult `shouldBe` Right "test-dir"
      
      let effectsResult = run $ runReader (defaultConfig "test-dir") effectsComputation
      effectsResult `shouldBe` "test-dir"
      
    it "handles errors consistently between systems" $ do
      let mtlError = throwError (ConfigError "test error") :: ClodM String
          effectsError = throw (ConfigError "test error") :: Member (Error ClodError) r => Sem r String
          
      mtlResult <- runExceptT $ runReaderT mtlError (defaultConfig "test")
      mtlResult `shouldBe` Left (ConfigError "test error")
      
      let effectsResult = run $ runError effectsError :: Either ClodError String
      effectsResult `shouldBe` Left (ConfigError "test error")
  
  where
    defaultConfig dir = ClodConfig 
      { projectDir = dir
      , stagingDir = dir </> "staging"
      , currentStaging = dir </> "staging"
      , lastRunFile = dir </> ".clod-last-run"
      , ignorePatterns = []
      , useGit = False
      }