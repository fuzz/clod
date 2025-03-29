{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Clod.EffectsSpec
-- Description : Tests for the effects system
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the algebraic effects system.

module Clod.EffectsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.ByteString as BS
import Data.Either (isRight, isLeft)
import Control.Monad (forM_, when)

import Polysemy
import Polysemy.Error
import Polysemy.Reader

import Clod.Types (ClodError(..), ClodConfig(..))
import Clod.Effects

-- | A test effect for unit testing
data TestEffect m a where
  GetValue :: TestEffect m Int
  SetValue :: Int -> TestEffect m ()

makeSem ''TestEffect

-- | A test interpreter that runs in pure memory
runTestPure :: Int -> Sem (TestEffect ': r) a -> Sem r (a, Int)
runTestPure initial = interpret $ \case
  GetValue -> pure initial
  SetValue v -> pure v

-- | Property: Effects should compose with the +> operator
prop_effectComposition :: Bool
prop_effectComposition = 
  let testFn :: (Member TestEffect r, Member (Reader Int) r) => Sem r Int
      testFn = do
        v1 <- getValue
        v2 <- ask
        pure (v1 + v2)
        
      result = run . runReader (5 :: Int) . fst . runTestPure 3 $ testFn
  in result == 8

spec :: Spec
spec = do
  describe "Effect composition" $ do
    it "should compose effects with +> operator" $ do
      property $ \(x :: Int) (y :: Int) ->
        let testFn :: (Member TestEffect r, Member (Reader Int) r) => Sem r Int
            testFn = do
              v1 <- getValue
              v2 <- ask
              pure (v1 + v2)
          
            result = run . runReader y . fst . runTestPure x $ testFn
        in result == (x + y)
  
  describe "FileSystem effects" $ do
    it "reads and writes files" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.txt"
            testContent = "Hello, effects!"
        
        result <- runM . runError . runFileSystemIO $ do
          writeFile testFile (BS.pack $ map fromIntegral $ fromEnum <$> testContent)
          fileExists testFile >>= \exists -> when (not exists) $ 
            throw $ FileSystemError "File should exist"
          content <- readFile testFile
          pure (BS.unpack content)
        
        case result of
          Left err -> fail $ "Should not have failed: " ++ show err
          Right bytes -> map toEnum bytes `shouldBe` testContent
  
  describe "Error handling" $ do
    it "catches and transforms errors" $ do
      let m1 = (throw (ConfigError "test error") :: Member (Error ClodError) r => Sem r ())
          m2 = catch m1 (\(ConfigError _) -> pure "caught")
          
      result <- runM . runError $ m2
      result `shouldBe` Right "caught"
  
  describe "Console effects" $ do
    it "can log information" $ do
      let mockLog = interpret $ \case
            LogInfo _ -> pure ()
            LogWarning _ -> pure ()
            LogError _ -> pure ()
            LogOutput _ -> pure ()
            
          program = do
            logInfo "info message"
            logWarning "warning message"
            logError "error message"
            logOutput "output message"
            pure True
            
      result <- runM $ mockLog program
      result `shouldBe` True
  
  describe "Effect stack" $ do
    it "runs a full effect stack" $ do
      withSystemTempDirectory "clod-test" $ \tmpDir -> do
        let config = ClodConfig
              { projectDir = tmpDir
              , stagingDir = tmpDir </> "staging"
              , currentStaging = tmpDir </> "staging"
              , lastRunFile = tmpDir </> ".clod-last-run"
              , ignorePatterns = []
              , useGit = False
              }
        
        createDirectoryIfMissing True (tmpDir </> "staging")
        
        result <- runClodEffects config $ do
          logInfo "Running test"
          pure "success"
        
        result `shouldBe` Right "success"