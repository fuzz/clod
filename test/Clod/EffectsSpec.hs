{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Clod.EffectsSpec
-- Description : Test for the effects module - now using the ClodM monad stack
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This file has been updated to use the new ClodM monad stack instead of
-- the Polysemy effects system.

module Clod.EffectsSpec (spec) where

import Test.Hspec
import Control.Monad.Except ()
import Control.Monad.Reader ()

import Clod.Types

-- | Test the error handling with the simplified monad stack
spec :: Spec
spec = do
  describe "Error handling" $ do
    it "can throw and catch errors" $ do
      let config = ClodConfig 
            { projectPath = "/"
            , stagingDir = "/"
            , configDir = "/"
            , databaseFile = "/.clod/database.dhall"
            , previousStaging = Nothing
            , flushMode = False
            , lastMode = False
            , timestamp = ""
            , currentStaging = "/"
            , testMode = True
            , verbose = False
            , ignorePatterns = []
            }
      
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
      let config = ClodConfig 
            { projectPath = "/"
             stagingDir = "/"
             configDir = "/"
             databaseFile = tmpDir </> ".clod" </> "database.dhall",
  previousStaging = Nothing,
  flushMode = False,
  lastMode = False,
             timestamp = ""
             currentStaging = "/"
             testMode = True
             verbose = False
             ignorePatterns = []
            }
      
      -- Create a monad that throws an error but doesn't catch it
      let action = throwError (ConfigError "test error") :: ClodM ()
      
      -- Run the action and check the result
      result <- runClodM config action
      result `shouldBe` Left (ConfigError "test error")