{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Main entry point for the Clod application
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides the main CLI interface for the Clod application.

module Main where

import Options.Applicative
import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)

import Clod.Core (runClod)
import Clod.Types (runClodM)

-- | Command line options for Clod
data Options = Options
  { optStagingDir :: String   -- ^ Directory where files will be staged
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
     <> help "Directory where files will be staged for Claude"
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
  result <- runClodM $ runClod 
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