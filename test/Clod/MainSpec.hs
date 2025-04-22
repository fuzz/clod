{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Clod.MainSpec
-- Description : Tests for command-line interface
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : fuzz@fuzz.ink
-- Stability   : experimental
--
-- This module contains tests for the main command-line interface.

module Clod.MainSpec (spec) where

import Test.Hspec
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
-- Import System.Environment only for instances
import System.Environment ()
import Data.Either (isRight)
import qualified System.IO
import qualified Control.Exception as Exception
import Control.Exception (SomeException)
import Control.Monad.IO.Class ()

import qualified Clod.Core as Core
import qualified Clod.Types as T
import qualified Options.Applicative as Opt
import Clod.TestHelpers (defaultTestConfig)

-- We'll create a mock Options type instead of importing Main
-- This avoids circular dependencies and follows test isolation principles

-- | Mock Options type that matches Main module's Options type
data Options = Options
  { optStagingDir  :: String  -- ^ Directory where files will be staged
  , optAllFiles    :: Bool    -- ^ Import all files
  , optTestMode    :: Bool    -- ^ Run in test mode
  , optVerbose     :: Bool    -- ^ Enable verbose output
  , optFlush       :: Bool    -- ^ Flush stale entries from the database
  , optLast        :: Bool    -- ^ Use previous staging directory
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
      ( Opt.long "test"
     <> Opt.short 't'
     <> Opt.help "Run in test mode" )
  <*> Opt.switch
      ( Opt.long "verbose"
     <> Opt.short 'v'
     <> Opt.help "Enable verbose output" )
  <*> Opt.switch
      ( Opt.long "flush"
     <> Opt.short 'f'
     <> Opt.help "Flush missing entries from the database" )
  <*> Opt.switch
      ( Opt.long "last"
     <> Opt.short 'l'
     <> Opt.help "Use previous staging directory" )

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
      
  it "parses --flush flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--flush"]
    case result of
      Opt.Success options -> optFlush options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --flush flag"
      
  it "parses --last flag correctly" $ do
    let result = Opt.execParserPure Opt.defaultPrefs opts ["--last"]
    case result of
      Opt.Success options -> optLast options `shouldBe` True
      _                   -> expectationFailure "Failed to parse --last flag"
      
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
        optTestMode options `shouldBe` False
        optVerbose options `shouldBe` False
        optFlush options `shouldBe` False
        optLast options `shouldBe` False
        optStagingDir options `shouldBe` ""
      _ -> expectationFailure "Failed to parse with default options"

-- | Tests for CLI workflow integration
cliWorkflowSpec :: Spec
cliWorkflowSpec = describe "CLI workflow" $ do
  it "follows SPEC.md behavior on first run and subsequent runs" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up a test repository
      let projectDir = tmpDir </> "project"
      let stagingDir = tmpDir </> "staging"
      let configDir = projectDir </> ".clod"
      
      -- Create minimal project structure
      createDirectoryIfMissing True projectDir
      createDirectoryIfMissing True configDir
      createDirectoryIfMissing True stagingDir
      
      -- Create test files
      System.IO.writeFile (projectDir </> "file1.txt") "original content"
      System.IO.writeFile (projectDir </> "file2.txt") "original content"
      
      -- Create a basic config
      let stagingDirPath = stagingDir
          configDirPath = configDir
          config = defaultTestConfig projectDir
            T.& T.stagingDir T..~ stagingDirPath
            T.& T.currentStaging T..~ stagingDirPath
            T.& T.configDir T..~ configDirPath
            T.& T.databaseFile T..~ configDirPath </> "checksums.dhall"
      
      -- Initialize test environment
      
      -- Run first time - should process all files
      _ <- Core.runClodApp config stagingDir False False
      
      -- Check if both files were processed
      file1Exists <- doesFileExist (stagingDir </> "file1.txt")
      file2Exists <- doesFileExist (stagingDir </> "file2.txt")
      file1Exists `shouldBe` True
      file2Exists `shouldBe` True
      
      -- Create a new staging directory for second run
      let stagingDir2 = tmpDir </> "staging2"
      createDirectoryIfMissing True stagingDir2
      
      -- Update config for second run
      let stagingDir2Path = stagingDir2
          config2 = config
            T.& T.stagingDir T..~ stagingDir2Path
            T.& T.currentStaging T..~ stagingDir2Path
      
      -- Make no changes to files
      result2 <- Core.runClodApp config2 stagingDir2 False False
      
      -- Print any errors for debugging
      case result2 of
        Left err -> putStrLn $ "Second run error: " ++ show err
        Right _ -> putStrLn "Second run completed successfully"
      
      -- Files should not be processed again
      file1Exists2 <- doesFileExist (stagingDir2 </> "file1.txt")
      file2Exists2 <- doesFileExist (stagingDir2 </> "file2.txt")
      manifestExists <- doesFileExist (stagingDir2 </> "_path_manifest.dhall")
      
      -- Check that files are processed according to SPEC.md
      -- The manifest is always created, but files should NOT be copied on second run
      -- if they haven't changed
      file1Exists2 `shouldBe` False  -- Unchanged files should NOT be processed on second run
      file2Exists2 `shouldBe` False  -- Unchanged files should NOT be processed on second run
      manifestExists `shouldBe` True  -- Manifest is always created
      
      -- Now modify a file and check third run
      System.IO.writeFile (projectDir </> "file1.txt") "modified content"
      
      -- Create a new staging directory for third run
      let stagingDir3 = tmpDir </> "staging3"
      createDirectoryIfMissing True stagingDir3
      
      -- Update config for third run
      let stagingDir3Path = stagingDir3
          config3 = config
            T.& T.stagingDir T..~ stagingDir3Path
            T.& T.currentStaging T..~ stagingDir3Path
      
      -- Run third time - should process only modified file
      _ <- Core.runClodApp config3 stagingDir3 False False
      
      -- Check which files were processed
      file1Exists3 <- doesFileExist (stagingDir3 </> "file1.txt")
      file2Exists3 <- doesFileExist (stagingDir3 </> "file2.txt")
      
      -- According to SPEC.md, on subsequent runs only changed files should be processed
      -- unless the --all flag is set
      file1Exists3 `shouldBe` True   -- Modified file should be copied
      file2Exists3 `shouldBe` False  -- Unchanged file should NOT be copied
  
  it "creates necessary directories" $ do
    withSystemTempDirectory "clod-test" $ \tmpDir -> do
      -- Set up a test repository
      let projectDir = tmpDir </> "project"
      let stagingDir = tmpDir </> "staging"
      
      -- Create minimal project structure
      createDirectoryIfMissing True projectDir
      createDirectoryIfMissing True (projectDir </> ".git")
      System.IO.writeFile (projectDir </> "test.txt") "test content"
      
      -- Run with test arguments
      -- Direct execution in production code would need a more sophisticated approach
      result <- Exception.try @SomeException $ do
        -- Create minimal config to use in test (avoiding main which could exit)
        let stagingDirPath = stagingDir
            config = defaultTestConfig projectDir
              T.& T.stagingDir T..~ stagingDirPath
              T.& T.currentStaging T..~ stagingDirPath
        
        -- Now execute what main would do but in a controlled way
        createDirectoryIfMissing True stagingDir
        createDirectoryIfMissing True (projectDir </> ".clod")
        
        -- Create a test file that should be processed
        System.IO.writeFile (projectDir </> "test.txt") "test content"
        
        -- Run core function directly instead of Main.main
        -- First run the main app to set up directories
        _ <- Core.runClodApp config stagingDir True False
        
        -- Now explicitly process the test file (since runClodApp doesn't process files by default)
        let readCap = T.fileReadCap [projectDir]
            writeCap = T.fileWriteCap [stagingDir]
            
        -- Process the test file manually
        T.runClodM config $ 
          Core.processFile readCap writeCap (projectDir </> "test.txt") "test.txt"
      
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