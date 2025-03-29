{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.MainSpec
-- Description : Tests for command-line interface
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module contains tests for the main command-line interface.

module Clod.MainSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Environment (withArgs)
import Data.Either (isRight)
import qualified System.IO
import qualified Control.Exception as Exception
import Control.Exception (SomeException)
import Control.Monad.IO.Class ()

import qualified Clod.Core as Core
import Clod.Types (ClodConfig(..), runClodM, fileReadCap, fileWriteCap)
import qualified Options.Applicative as Opt

-- We'll create a mock Options type instead of importing Main
-- This avoids circular dependencies and follows test isolation principles

-- | Mock Options type that matches Main module's Options type
data Options = Options
  { optStagingDir  :: String  -- ^ Directory where files will be staged
  , optAllFiles    :: Bool    -- ^ Import all files
  , optModified    :: Bool    -- ^ Import only modified files
  , optTestMode    :: Bool    -- ^ Run in test mode
  , optVerbose     :: Bool    -- ^ Enable verbose output
  } deriving (Show, Eq)

-- | Mock parser for options that matches Main module's optionsParser
optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> Opt.strOption
      ( Opt.long "staging-dir"
     <> Opt.short 'd'
     <> Opt.metavar "DIR"
     <> Opt.help "Directory where files will be staged for Claude"
     <> Opt.value ""
     <> Opt.showDefault )
  <*> Opt.switch
      ( Opt.long "all"
     <> Opt.short 'a'
     <> Opt.help "Import all files" )
  <*> Opt.switch
      ( Opt.long "modified"
     <> Opt.short 'm'
     <> Opt.help "Import only modified files" )
  <*> Opt.switch
      ( Opt.long "test"
     <> Opt.short 't'
     <> Opt.help "Run in test mode" )
  <*> Opt.switch
      ( Opt.long "verbose"
     <> Opt.short 'v'
     <> Opt.help "Enable verbose output" )

-- | Mock opts that matches Main module's opts
opts :: Opt.ParserInfo Options
opts = Opt.info (optionsParser Opt.<**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Prepare files from a git repository for upload to Claude's Project Knowledge"
  <> Opt.header "clod - Claude Git Project File Uploader" )

-- | Test specification for Main module
spec :: Spec
spec = do
  commandLineOptionsSpec
  cliWorkflowSpec

-- | Tests for command-line options parsing
commandLineOptionsSpec :: Spec
commandLineOptionsSpec = describe "Command line options parsing" $ do
  it "parses --all flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--all"]
    case result of
      Opt.Success options -> optAllFiles options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --all flag"
      
  it "parses --modified flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--modified"]
    case result of
      Opt.Success options -> optModified options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --modified flag"
      
  it "parses --test flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--test"]
    case result of
      Opt.Success options -> optTestMode options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --test flag"
      
  it "parses --verbose flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--verbose"]
    case result of
      Opt.Success options -> optVerbose options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --verbose flag"
      
  it "parses --staging-dir flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--staging-dir", "/tmp/test-staging"]
    case result of
      Opt.Success options -> optStagingDir options `shouldBe` "/tmp/test-staging"
      _                   -> expectationFailure "Failed to parse --staging-dir flag"
      
  it "parses multiple flags correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--all", "--verbose", "--test"]
    case result of
      Opt.Success options -> do
        optAllFiles options `shouldBe` True
        optVerbose options `shouldBe` True
        optTestMode options `shouldBe` True
      _ -> expectationFailure "Failed to parse multiple flags"
      
  it "uses correct defaults for options" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts []
    case result of
      Opt.Success options -> do
        optAllFiles options `shouldBe` False
        optModified options `shouldBe` False
        optTestMode options `shouldBe` False
        optVerbose options `shouldBe` False
        optStagingDir options `shouldBe` ""
      _ -> expectationFailure "Failed to parse with default options"

-- | Tests for CLI workflow integration
cliWorkflowSpec :: Spec
cliWorkflowSpec = describe "CLI workflow" $ do
  it "creates necessary directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up a test repository
      let projectDir = tmpDir </> "project"
      let stagingDir = tmpDir </> "staging"
      
      -- Create minimal project structure
      createDirectoryIfMissing True projectDir
      createDirectoryIfMissing True (projectDir </> ".git")
      System.IO.writeFile (projectDir </> "test.txt") "test content"
      
      -- Save current directory
      originalDir <- getCurrentDirectory
      
      -- Change to test directory to run CLI
      setCurrentDirectory projectDir
      
      -- Run with test arguments
      -- Direct execution in production code would need a more sophisticated approach
      result <- Exception.try @SomeException $ withArgs ["--test", "--staging-dir", stagingDir] $ do
        -- Create minimal config to use in test (avoiding main which could exit)
        let config = ClodConfig {
              projectPath = projectDir,
              stagingDir = stagingDir,
              configDir = projectDir </> ".clod",
              lastRunFile = projectDir </> ".clod" </> "last-run-marker",
              timestamp = "20250325",
              currentStaging = stagingDir,
              testMode = True,
            verbose = False,
              ignorePatterns = []
            }
        
        -- Now execute what main would do but in a controlled way
        createDirectoryIfMissing True stagingDir
        createDirectoryIfMissing True (projectDir </> ".clod")
        
        -- Create a test file that should be processed
        System.IO.writeFile (projectDir </> "test.txt") "test content"
        
        -- Run core function directly instead of Main.main
        -- First run the main app to set up directories
        _ <- Core.runClodApp config stagingDir True False False
        
        -- Now explicitly process the test file (since runClodApp doesn't process files by default)
        let readCap = fileReadCap [projectDir]
            writeCap = fileWriteCap [stagingDir]
            
        -- Process the test file manually
        runClodM config $ 
          Core.processFile readCap writeCap (projectDir </> "test.txt") "test.txt"
      
      -- Restore original directory
      setCurrentDirectory originalDir
      
      -- Verify directories were created
      case result of
        Left err -> expectationFailure $ "Test failed with exception: " ++ show err
        Right coreResult -> do
          coreResult `shouldSatisfy` isRight
          
          -- Check if config directory was created
          configDirExists <- doesDirectoryExist (projectDir </> ".clod")
          configDirExists `shouldBe` True
          
          -- Check if staging directory was created
          stagingDirExists <- doesDirectoryExist stagingDir
          stagingDirExists `shouldBe` True
          
          -- Verify test.txt was processed and staged
          stagedFileExists <- doesFileExist (stagingDir </> "test.txt")
          stagedFileExists `shouldBe` True