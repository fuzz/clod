{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Clod.EffectsSpec
-- Description : Tests for the effects module using the ClodM monad stack
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for the ClodM monad stack, focusing on
-- error handling and basic monad operations.

module Clod.EffectsSpec (spec) where

import Test.Hspec
import Control.Monad.Except ()
import Control.Monad.Reader ()

import Clod.Types
import Clod.TestHelpers (defaultTestConfig)

-- | Test the error handling with the simplified monad stack
spec :: Spec
spec = do
  describe "Error handling" $ do
    it "can throw and catch errors" $ do
      let config = defaultTestConfig "/"
      
      -- Create a monad that throws and catches an error
      let action = do
            -- Throw an error
            throwError (ConfigError "test error")
          
          catchingAction = do
            -- Catch the specific error type
            catchError action $ \case
              ConfigError _ -> return ()
              _ -> throwError (ConfigError "wrong error type")
      
      -- Run the action and check the result
      result <- runClodM config catchingAction
      result `shouldBe` Right ()
      
    it "propagates uncaught errors" $ do
      let config = defaultTestConfig "/"
      
      -- Create a monad that throws an error but doesn't catch it
      let action = throwError (ConfigError "test error") :: ClodM ()
      
      -- Run the action and check the result
      result <- runClodM config action
      result `shouldBe` Left (ConfigError "test error")