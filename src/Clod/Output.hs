{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Output
-- Description : User interface and output formatting
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for user interaction and output formatting.
-- It handles terminal output with consistent styling, user prompts, and
-- guidance for next steps after file processing.
--
-- The module uses ANSI color codes to provide a clear, color-coded interface:
--
-- * Green for success messages
-- * Red for error messages
-- * Yellow for warnings and headers
--
-- It also provides interactive prompts for users to make decisions
-- and displays next steps for using the processed files with Claude AI.

module Clod.Output
  ( -- * Terminal output
    printHeader
  , printSuccess
  , printError
  , printWarning
  
    -- * User interaction
  , promptUser
  , promptYesNo
  
    -- * Next steps
  , showNextSteps
  ) where

import Control.Monad (unless)
import System.IO (hFlush, stdout)

import Clod.Types

-- | ANSI color codes for pretty printing
red, green, yellow, noColor :: String
red = "\ESC[0;31m"
green = "\ESC[0;32m"
yellow = "\ESC[1;33m"
noColor = "\ESC[0m"

-- | Print a header message in yellow
printHeader :: String -> ClodM ()
printHeader msg = liftIO $ putStrLn $ yellow ++ msg ++ noColor

-- | Print a success message in green
printSuccess :: String -> ClodM ()
printSuccess msg = liftIO $ putStrLn $ green ++ "✓ " ++ msg ++ noColor

-- | Print an error message in red
printError :: String -> ClodM ()
printError msg = liftIO $ putStrLn $ red ++ "✗ " ++ msg ++ noColor

-- | Print a warning message in yellow
printWarning :: String -> ClodM ()
printWarning msg = liftIO $ putStrLn $ yellow ++ "! " ++ msg ++ noColor

-- | Prompt the user for input with a default value
--
-- This function displays a prompt with a default value in brackets.
-- If the user presses Enter without typing anything, the default value is used.
--
-- @
-- -- Prompt for staging directory with default
-- stagingDir <- promptUser "Staging directory" "/home/user/Claude"
-- @
promptUser :: String  -- ^ The prompt to display
           -> String  -- ^ Default value to use if user input is empty
           -> ClodM String  -- ^ The user's input or default value
promptUser prompt defaultValue = do
  liftIO $ putStr $ prompt ++ " [" ++ defaultValue ++ "]: "
  liftIO $ hFlush stdout
  response <- liftIO getLine
  return $ if null response then defaultValue else response

-- | Prompt the user for a yes/no response
promptYesNo :: String -> Bool -> ClodM Bool
promptYesNo prompt defaultYes = do
  let defaultStr = if defaultYes then "Y/n" else "y/N"
  liftIO $ putStr $ prompt ++ " [" ++ defaultStr ++ "]: "
  liftIO $ hFlush stdout
  response <- liftIO getLine
  return $ case response of
    "" -> defaultYes
    r  -> (r `elem` ["y", "Y", "yes", "Yes", "YES"])

-- | Show next steps for using the processed files with Claude AI
--
-- This function displays guidance on how to use the processed files
-- with Claude AI's Project Knowledge feature. It's shown after successful
-- file processing, unless in test mode.
--
-- The instructions cover:
--
-- * Navigating to Project Knowledge in Claude
-- * Uploading files from the staging folder
-- * Using the path manifest to understand file origins
-- * Adding project instructions
-- * Managing file versions
--
-- @
-- -- Show next steps after processing files
-- showNextSteps config stagingDir
-- @
showNextSteps :: ClodConfig  -- ^ Program configuration
              -> FilePath    -- ^ Path to the staging directory
              -> ClodM ()
showNextSteps config _ = unless (testMode config) $ 
  mapM_ (liftIO . putStrLn) $ [""] ++ steps ++ [""] ++ notes
  where
    -- Numbered steps for Claude integration
    steps = zipWith formatStep [1..] 
      [ "Navigate to Project Knowledge in your Claude Project (Pro or Team account required)"
      , "Drag files from the staging folder to Project Knowledge"
      , "Don't forget _path_manifest.json which maps optimized names back to original paths"
      , "Paste the contents of project-instructions.md into the Project Instructions section"
      , "IMPORTANT: You must manually delete previous versions of these files from Project Knowledge\n   before starting a new conversation to ensure Claude uses the most recent files"
      , "Start a new conversation to see changes"
      ]
      
    -- Notes about staging directory
    notes = 
      [ "Note: The staging directory is temporary"
      , "      and will be cleaned up on your next run of clod (or system reboot)."
      ]
      
    -- Helper function to format numbered steps
    formatStep :: Int -> String -> String
    formatStep 1 text = "Next steps:\n1. " ++ text
    formatStep n text = show n ++ ". " ++ text