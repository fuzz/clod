{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clod.TypesSpec
-- Description : Tests for core types
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for the core types of the Clod application.

module Clod.TypesSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Success)
import Data.Text ()
import System.IO.Temp ()
import Control.Monad.Reader ()
import Control.Monad.Except ()

import Clod.Types
import Clod.TestHelpers (defaultTestConfig)

-- | Property: OptimizedName should preserve its structure through the newtype
prop_optimizedNameRoundTrip :: String -> Bool
prop_optimizedNameRoundTrip s = unOptimizedName (OptimizedName s) == s

-- | Property: OriginalPath should preserve its structure through the newtype
prop_originalPathRoundTrip :: FilePath -> Bool 
prop_originalPathRoundTrip p = unOriginalPath (OriginalPath p) == p

-- | Property: Validated to Either should preserve success values
prop_validatedToEitherSuccess :: Int -> Bool
prop_validatedToEitherSuccess x = validatedToEither (Valid x) == Right x

-- | Property: Either to Validated should preserve Right values
prop_eitherToValidatedRight :: Int -> Bool
prop_eitherToValidatedRight x = eitherToValidated (Right x) == Valid x

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
      show (DatabaseError "db.dhall" (DBCorrupted "parse error")) `shouldBe` "DatabaseError \"db.dhall\" (DBCorrupted \"parse error\")"
      show (CapabilityError "/path/to/file" "Access denied") `shouldBe` "CapabilityError \"/path/to/file\" \"Access denied\""
      show (ChecksumError "data.bin" "Binary file") `shouldBe` "ChecksumError \"data.bin\" \"Binary file\""
      
    it "handles all database error types" $ do
      show (DatabaseError "db" DBFileNotFound) `shouldBe` "DatabaseError \"db\" DBFileNotFound"
      show (DatabaseError "db" (DBCorrupted "parse error")) `shouldBe` "DatabaseError \"db\" (DBCorrupted \"parse error\")"
      show (DatabaseError "db" DBVersionMismatch) `shouldBe` "DatabaseError \"db\" DBVersionMismatch"
      show (DatabaseError "db" (DBOtherError "unknown error")) `shouldBe` "DatabaseError \"db\" (DBOtherError \"unknown error\")"
  
  describe "FileResult" $ do
    it "can be created and displayed" $ do
      show (Success) `shouldBe` "Success"
      show (Skipped "reason") `shouldBe` "Skipped \"reason\""
  
  describe "ClodM Monad Stack" $ do
    it "handles reader operations correctly" $ do
      let mtlComputation = do
            config <- ask
            return (config ^. projectPath) :: ClodM String
            
      mtlResult <- runExceptT $ runReaderT mtlComputation (defaultTestConfig "test-dir")
      mtlResult `shouldBe` Right "test-dir"
      
    it "handles errors correctly" $ do
      let mtlError = throwError (ConfigError "test error") :: ClodM String
          
      mtlResult <- runExceptT $ runReaderT mtlError (defaultTestConfig "test")
      mtlResult `shouldBe` Left (ConfigError "test error")
      
  describe "Validated data type" $ do
    it "converts from Valid to Right" $ do
      property prop_validatedToEitherSuccess
      
    it "converts from Invalid to Left (first error)" $ do
      -- Test with a specific case instead of property
      let e1 = ConfigError "main error"
      let e2 = ConfigError "secondary error"
      validatedToEither (Invalid [e1, e2] :: Validated String) `shouldBe` (Left e1 :: Either ClodError String)
      
    it "converts from Right to Valid" $ do
      property prop_eitherToValidatedRight
      
    it "converts from Left to Invalid (singleton)" $ do
      -- Test with a specific case
      let err = ConfigError "test error"
      eitherToValidated (Left err :: Either ClodError String) `shouldBe` (Invalid [err] :: Validated String)
      
    it "combines errors when applying Invalid values" $ do
      -- Test with specific cases
      let es1 = [ConfigError "error 1"]
      let es2 = [ConfigError "error 2"]
      let v1 = Invalid es1 :: Validated (Int -> Int)
      let v2 = Invalid es2 :: Validated Int
      
      case v1 <*> v2 of
        Invalid combined -> combined `shouldBe` es1 ++ es2
        _ -> expectationFailure "Expected Invalid result"
      
    it "handles basic Applicative operations correctly" $ do
      let v1 = Valid (+1) :: Validated (Int -> Int)
      let v2 = Valid 2 :: Validated Int
      
      (v1 <*> v2) `shouldBe` Valid 3
      
      let e1 = Invalid [ConfigError "error 1"] :: Validated (Int -> Int)
      let e2 = Invalid [ConfigError "error 2"] :: Validated Int
      
      (v1 <*> e2) `shouldBe` e2
      
      -- Can't directly compare here because types differ - test the core behavior instead
      case (e1 <*> v2) of
        Invalid [ConfigError msg] -> msg `shouldBe` "error 1"
        _ -> expectationFailure "Expected Invalid with ConfigError"
      
      -- Test the combination of errors
      (e1 <*> e2) `shouldBe` Invalid [ConfigError "error 1", ConfigError "error 2"]
  
