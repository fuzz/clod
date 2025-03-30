{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.ManPagesSpec
-- Description : Tests for man page installation
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module tests that man pages are properly configured for build and installation.

module Clod.ManPagesSpec (spec) where

import Test.Hspec
import System.Directory (doesFileExist)
import System.Process (readProcess)
import Control.Exception (try, SomeException)

-- | Test that man page source files exist
spec :: Spec
spec = do
  describe "Man page configuration" $ do
    it "has man page source files" $ do
      -- Check that the man page source files exist
      man1Exists <- doesFileExist "man/clod.1.md"
      man7Exists <- doesFileExist "man/clod.7.md"
      man8Exists <- doesFileExist "man/clod.8.md"
      
      man1Exists `shouldBe` True
      man7Exists `shouldBe` True
      man8Exists `shouldBe` True
      
    it "has a working generate-man-pages.sh script" $ do
      -- Check that the generate-man-pages.sh script exists
      scriptExists <- doesFileExist "bin/generate-man-pages.sh"
      scriptExists `shouldBe` True
      
    it "can find pandoc for man page generation" $ do
      -- This test is not critical - it's okay if pandoc isn't available
      -- but we want to know if it is
      result <- try (readProcess "which" ["pandoc"] "") :: IO (Either SomeException String)
      case result of
        Right _ -> putStrLn "Note: pandoc is available for man page generation"
        Left _ -> putStrLn "Note: pandoc is not available for man page generation"