{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Main entry point for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides the main CLI interface for the Clod application.

module Main where

import Options.Applicative
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import System.Environment (lookupEnv)

import Clod.Core (runClodApp)
import Clod.Types (ClodConfig(..))

-- | Command line options for Clod
data Options = Options
  { optStagingDir :: String   -- ^ Directory where files will be staged (test mode only)
  , optAllFiles   :: Bool     -- ^ Import all files
  , optModified   :: Bool     -- ^ Import only modified files
  , optTestMode   :: Bool     -- ^ Run in test mode
  , optVerbose    :: Bool     -- ^ Enable verbose output
  } deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "staging-dir"
     <> short 'd'
     <> metavar "DIR"
     <> help "Directory where files will be staged for Claude (only used in test mode)"
     <> value ""
     <> showDefault )
  <*> switch
      ( long "all"
     <> short 'a'
     <> help "Import all files (respecting .gitignore)" )
  <*> switch
      ( long "modified"
     <> short 'm'
     <> help "Import only modified files" )
  <*> switch
      ( long "test"
     <> short 't'
     <> help "Run in test mode" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output" )

-- | Main entry point
main :: IO ()
main = do
  options <- execParser opts
  
  -- Create a minimal configuration for the effects system
  currentDir <- getCurrentDirectory
  -- Default to XDG config home if environment variable is set, otherwise ~/.config
  xdgConfigHome <- lookupEnv "XDG_CONFIG_HOME"
  let configHome = maybe (getHomeDirectory >>= \home -> return $ home </> ".config") return xdgConfigHome
  configHomeDir <- configHome
  
  let configDir = if optTestMode options
                 then currentDir </> ".clod"
                 else configHomeDir </> "clod"
                 
  let stagingDirPath = if null (optStagingDir options) 
                    then configDir </> "staging"
                    else optStagingDir options
  
  -- Create staging directory if it doesn't exist
  createDirectoryIfMissing True stagingDirPath
  createDirectoryIfMissing True configDir
  
  -- Create a basic config
  let config = ClodConfig {
        projectPath = currentDir,
        stagingDir = stagingDirPath,
        configDir = configDir,
        lastRunFile = configDir </> "last-run-marker",
        timestamp = "",  -- Will be set internally
        currentStaging = stagingDirPath,
        testMode = optTestMode options,
        ignorePatterns = []  -- Will be populated
      }
  
  -- Run with effects system
  result <- runClodApp config 
              (optStagingDir options)
              (optAllFiles options)
              (optModified options)
              (optTestMode options)
  
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right _ -> return ()
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Prepare files from a git repository for upload to Claude's Project Knowledge"
     <> header "clod - Claude Git Project File Uploader" )