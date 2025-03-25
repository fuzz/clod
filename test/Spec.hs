{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Spec
-- Description : Test suite for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module orchestrates the test suite for the Clod application.

module Main where

import Test.Hspec

import qualified Clod.IgnorePatternsSpec
import qualified Clod.GitSpec
import qualified Clod.FileSystemSpec
import qualified Clod.CoreSpec

main :: IO ()
main = hspec $ do
  describe "Clod.IgnorePatterns" Clod.IgnorePatternsSpec.spec
  describe "Clod.Git" Clod.GitSpec.spec
  describe "Clod.FileSystem" Clod.FileSystemSpec.spec
  describe "Clod.Core" Clod.CoreSpec.spec