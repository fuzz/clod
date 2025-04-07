{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Main entry point for the Clod application
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : ink@fuzz.ink
-- Stability   : experimental
--
-- This module provides the main CLI interface for the Clod application.

module Main where

import Options.Applicative
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getTemporaryDirectory, 
                         doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO (stderr, hPutStrLn)
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Hashable (hash)
import Control.Exception (try, IOException)
import Data.List (isInfixOf)
import Data.Version (showVersion)
import qualified Paths_clod as Meta

import Clod.Core (runClodApp)
import Clod.Types (ClodConfig(..))

-- | Command line options for Clod
data Options = Options
  { optStagingDir  :: String   -- ^ Directory where files will be staged (test mode only)
  , optAllFiles    :: Bool     -- ^ Import all files (otherwise imports only modified files)
  , optTestMode    :: Bool     -- ^ Run in test mode
  , optVerbose     :: Bool     -- ^ Enable verbose output
  , optFlush       :: Bool     -- ^ Flush stale entries from the database
  , optLast        :: Bool     -- ^ Use previous staging directory
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
      ( long "test"
     <> short 't'
     <> help "Run in test mode" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output" )
  <*> switch
      ( long "flush"
     <> short 'f'
     <> help "Flush missing entries from the database" )
  <*> switch
      ( long "last"
     <> short 'l'
     <> help "Use previous staging directory" )

-- | Main entry point
main :: IO ()
main = do
  options <- execParser opts
  
  -- Create a minimal configuration for the effects system
  currentDir <- getCurrentDirectory
  
  -- For config dir: use local .clod in project directory
  let configDir = currentDir </> ".clod"
  
  -- For staging directory: use system temp directory
  tempDir <- getTemporaryDirectory
  let stagingDirBase = tempDir </> "clod-staging"
  
  -- Create a unique staging directory for this run
  timestamp <- formatTime defaultTimeLocale "%Y%m%d%H%M%S" <$> getCurrentTime
  let uniqueId = take 8 $ show $ hash $ currentDir ++ timestamp
  let stagingDirPath = if null (optStagingDir options) 
                    then stagingDirBase </> uniqueId
                    else optStagingDir options
  
  -- Load previous staging directory if in "last" mode
  let dbPath = configDir </> "db.dhall"
  previousDir <- if optLast options
                 then do
                   dbExists <- doesFileExist dbPath
                   if dbExists
                     then do
                       -- Try to read the database to get the previous staging dir
                       edb <- try $ do
                         content <- readFile dbPath
                         -- Look for lastStagingDir pattern in the Dhall file
                         let contentLines = filter (\l -> "lastStagingDir" `isInfixOf` l) $ lines content
                         case contentLines of
                           (line:_) -> 
                             if "None" `isInfixOf` line
                               then return Nothing 
                               else do
                                 -- Extract path from "Some ./path/to/dir" format
                                 let parts = words line
                                 let path = if length parts > 2 
                                            then read (last parts) :: String
                                            else ""
                                 return $ if null path then Nothing else Just path
                           _ -> return Nothing
                         :: IO (Either IOException (Maybe FilePath))
                       case edb of
                         Left _ -> return Nothing
                         Right mbDir -> return mbDir
                     else return Nothing
                 else return Nothing
  
  -- Use previous staging dir if requested and available
  finalStagingPath <- case (optLast options, previousDir) of
                        (True, Just prevDir) -> do
                          -- Check if the previous directory still exists
                          dirExists <- doesDirectoryExist prevDir
                          return $ if dirExists 
                                   then prevDir
                                   else stagingDirPath
                        _ -> return stagingDirPath
  
  do
      -- Create staging directory if it doesn't exist
      createDirectoryIfMissing True finalStagingPath
      createDirectoryIfMissing True configDir
      
      -- Create a basic config
      let config = ClodConfig {
            projectPath = currentDir,
            stagingDir = finalStagingPath,
            configDir = configDir,
            databaseFile = dbPath,
            timestamp = "",  -- Will be set internally
            currentStaging = finalStagingPath,
            previousStaging = previousDir,
            testMode = optTestMode options,
            verbose = optVerbose options,
            flushMode = optFlush options,
            lastMode = optLast options,
            ignorePatterns = []  -- Will be populated
          }
      
      -- Run with effects system
      result <- runClodApp config 
                  (optStagingDir options)
                  (optVerbose options)
                  (optAllFiles options)
      
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ show err
          exitFailure
        Right _ -> return ()
  where
    opts = info (optionsParser <**> helper <**> version)
      ( fullDesc
     <> progDesc "Prepare files from a git repository for upload to Claude's Project Knowledge"
     <> header "clod - Claude Git Project File Uploader" )
    version = infoOption (showVersion Meta.version)
      ( long "version"
     <> short 'V'
     <> help "Show version information" )
