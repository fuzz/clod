{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Spec
-- Description : Test suite for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module orchestrates the test suite for the Clod application.

module Main where

import Test.Hspec

import qualified Clod.IgnorePatternsSpec
import qualified Clod.GitSpec
import qualified Clod.FileSystemSpec
import qualified Clod.CoreSpec
import qualified Clod.OutputSpec
import qualified Clod.ConfigSpec
import qualified Clod.MainSpec
import qualified Clod.FileSystem.DetectionSpec
import qualified Clod.CapabilitySpec

main :: IO ()
main = hspec $ do
  describe "Clod.IgnorePatterns" Clod.IgnorePatternsSpec.spec
  describe "Clod.Git" Clod.GitSpec.spec
  describe "Clod.FileSystem" Clod.FileSystemSpec.spec
  describe "Clod.Core" Clod.CoreSpec.spec
  describe "Clod.Output" Clod.OutputSpec.spec
  describe "Clod.Config" Clod.ConfigSpec.spec
  describe "Clod.Main" Clod.MainSpec.spec
  describe "Clod.FileSystem.Detection" Clod.FileSystem.DetectionSpec.spec
  describe "Clod.Capability" Clod.CapabilitySpec.spec
