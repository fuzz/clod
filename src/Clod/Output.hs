{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Clod.Output
-- Description : User interface and output formatting
-- Copyright   : (c) fuzz, 2025
-- License     : MIT
-- Maintainer  : fuzz@github.com
-- Stability   : experimental
--
-- This module provides functions for user interaction and output formatting.

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
import Control.Monad.IO.Class (liftIO)
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
promptUser :: String -> String -> ClodM String
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

-- | Show next steps for the user
showNextSteps :: ClodConfig -> FilePath -> ClodM ()
showNextSteps config _ = unless (testMode config) $ do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Next steps:"
  liftIO $ putStrLn "1. Navigate to Project Knowledge in your Claude Project (Pro or Team account required)"
  liftIO $ putStrLn "2. Drag files from the staging folder to Project Knowledge"
  liftIO $ putStrLn "3. Don't forget _path_manifest.json which maps optimized names back to original paths"
  liftIO $ putStrLn "4. Paste the contents of project-instructions.md into the Project Instructions section"
  liftIO $ putStrLn "5. IMPORTANT: You must manually delete previous versions of these files from Project Knowledge"
  liftIO $ putStrLn "   before starting a new conversation to ensure Claude uses the most recent files"
  liftIO $ putStrLn "6. Start a new conversation to see changes"