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
import qualified Clod.AdvancedCapabilitySpec
import qualified Clod.EffectsSpec
import qualified Clod.TypesSpec
import qualified Clod.FileSystem.OperationsSpec
import qualified Clod.FileSystem.ProcessingSpec
import qualified Clod.FileSystem.TransformationsSpec
import qualified Clod.Git.InternalSpec
import qualified Clod.Git.LibGitSpec

main :: IO ()
main = hspec $ do
  describe "Clod.IgnorePatterns" Clod.IgnorePatternsSpec.spec
  describe "Clod.Git" Clod.GitSpec.spec
  describe "Clod.FileSystem" Clod.FileSystemSpec.spec
  describe "Clod.FileSystem.Detection" Clod.FileSystem.DetectionSpec.spec
  describe "Clod.Core" Clod.CoreSpec.spec
  describe "Clod.Output" Clod.OutputSpec.spec
  describe "Clod.Config" Clod.ConfigSpec.spec
  describe "Clod.Main" Clod.MainSpec.spec
  describe "Clod.Capability" Clod.CapabilitySpec.spec
  describe "Clod.AdvancedCapability" Clod.AdvancedCapabilitySpec.spec
  describe "Clod.Effects" Clod.EffectsSpec.spec
  describe "Clod.Types" Clod.TypesSpec.spec
  describe "Clod.FileSystem.Operations" Clod.FileSystem.OperationsSpec.spec
  describe "Clod.FileSystem.Processing" Clod.FileSystem.ProcessingSpec.spec
  describe "Clod.FileSystem.Transformations" Clod.FileSystem.TransformationsSpec.spec
  describe "Clod.Git.Internal" Clod.Git.InternalSpec.spec
  describe "Clod.Git.LibGit" Clod.Git.LibGitSpec.spec
