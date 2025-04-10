{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Spec
-- Description : Test suite for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module orchestrates the test suite for the Clod application.

module Main where

import Test.Hspec

import qualified Clod.IgnorePatternsSpec
import qualified Clod.FileSystemSpec
import qualified Clod.CoreSpec
import qualified Clod.OutputSpec
import qualified Clod.ConfigSpec
import qualified Clod.MainSpec
import qualified Clod.FileSystem.DetectionSpec
import qualified Clod.FileSystem.ChecksumsSpec
import qualified Clod.FileSystem.DatabaseSpec
import qualified Clod.TypesSpec
import qualified Clod.FileSystem.OperationsSpec
import qualified Clod.FileSystem.ProcessingSpec
import qualified Clod.FileSystem.TransformationsSpec
import qualified Clod.ManPagesSpec

main :: IO ()
main = hspec $ do
  describe "Clod.IgnorePatterns" Clod.IgnorePatternsSpec.spec
  describe "Clod.FileSystem" Clod.FileSystemSpec.spec
  describe "Clod.FileSystem.Detection" Clod.FileSystem.DetectionSpec.spec
  describe "Clod.FileSystem.Checksums" Clod.FileSystem.ChecksumsSpec.spec
  describe "Clod.FileSystem.Database" Clod.FileSystem.DatabaseSpec.spec
  describe "Clod.Core" Clod.CoreSpec.spec
  describe "Clod.Output" Clod.OutputSpec.spec
  describe "Clod.Config" Clod.ConfigSpec.spec
  describe "Clod.Main" Clod.MainSpec.spec
  describe "Clod.Types" Clod.TypesSpec.spec
  describe "Clod.FileSystem.Operations" Clod.FileSystem.OperationsSpec.spec
  describe "Clod.FileSystem.Processing" Clod.FileSystem.ProcessingSpec.spec
  describe "Clod.FileSystem.Transformations" Clod.FileSystem.TransformationsSpec.spec
  describe "Clod.ManPages" Clod.ManPagesSpec.spec
