{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : EffectsExample
-- Description : Example of using the ClodM monad stack with AI-generated code
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides an example of how to use the ClodM monad stack with AI-generated code.
-- It demonstrates how capabilities can restrict what files the code can access.

module Main where

import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName, takeDirectory)
import qualified Data.ByteString.Char8 as BC
import Control.Monad.IO.Class (liftIO)

import Clod.Types 
  ( ClodConfig(..), ClodM, runClodM
  , FileReadCap(..), FileWriteCap(..), fileReadCap, fileWriteCap )
import Clod.FileSystem.Detection (safeIsTextFile, safeFileExists)
import Clod.FileSystem.Operations (safeReadFile, safeWriteFile)

-- | Example of AI-generated code with capability-based security
-- This module demonstrates how the capability-based security system works

-- | A malicious function that tries to access files outside allowed directories
-- This will fail at runtime when it attempts to access unallowed paths
maliciousFunction :: FileReadCap -> FileWriteCap -> ClodM ()
maliciousFunction readCap writeCap = do
  -- Attempt to read /etc/passwd (should fail with access denied)
  liftIO $ putStrLn "Attempting to access sensitive file..."
  content <- safeReadFile readCap "/etc/passwd"
  
  -- This code would never execute due to the capability check
  liftIO $ putStrLn $ "Successfully read sensitive data: " ++ show (BC.take 50 content)
  
  -- Attempt to write to /etc/passwd (should fail with access denied)
  safeWriteFile writeCap "/etc/passwd" "HACKED"
  
  liftIO $ putStrLn "Hack successful"  -- This would never execute

-- | Helper function to pass the output directory to the AI-generated function
aiGeneratedFunctionWithOutput :: FileReadCap -> FileWriteCap -> FilePath -> FilePath -> ClodM ()
aiGeneratedFunctionWithOutput readCap writeCap filePath outputDir = do
  -- First, we check if the file exists using capabilities
  fileExists' <- safeFileExists readCap filePath
  
  if not fileExists'
    then liftIO $ putStrLn $ "File does not exist: " ++ filePath
    else do
      -- Check if it's a text file using capabilities
      isText <- safeIsTextFile readCap filePath
      if not isText
        then liftIO $ putStrLn $ "File is binary, skipping content transformation: " ++ filePath
        else do
          -- Read the file content using capabilities
          content <- safeReadFile readCap filePath
          
          -- Log the content length
          liftIO $ putStrLn $ "Read file with " ++ show (BC.length content) ++ " bytes"
          
          -- Perform some transformation (this is pure code)
          let transformed = BC.map toUpper content
          
          -- Write to output file using capabilities
          let outputName = takeFileName filePath ++ ".transformed"
              fullOutputPath = outputDir </> outputName
          liftIO $ putStrLn $ "Writing transformed content to: " ++ fullOutputPath
          safeWriteFile writeCap fullOutputPath transformed
          
          liftIO $ putStrLn "Transformation complete!"
  where
    toUpper c | c >= 'a' && c <= 'z' = toEnum $ fromEnum c - 32
              | otherwise = c

main :: IO ()
main = do
  -- Get current directory
  currentDir <- getCurrentDirectory
  args <- getArgs
  
  -- Determine the file to process (default to README.md or use first arg)
  let targetFile = case args of
                     (file:_) -> file  -- Use first command line argument if provided
                     []       -> currentDir </> "README.md"  -- Default to README.md
  
  -- Use the parent "clod-contain" directory for outputs as that's what it's for
  -- This avoids polluting the git repository with test outputs
  let parentDir = takeDirectory currentDir
      outputDir = parentDir </> "output"
  createDirectoryIfMissing True outputDir
  putStrLn $ "Created output directory outside repo: " ++ outputDir
  
  -- Create capabilities that only allow access to specific directories
  -- Using absolute paths is critical for the capability system
  let readCap = fileReadCap [currentDir]
      writeCap = fileWriteCap [outputDir]
      
  -- Create a default config for the ClodM monad
  let config = ClodConfig
        { projectPath = currentDir
        , stagingDir = outputDir
        , configDir = currentDir </> ".clod"
        , lastRunFile = currentDir </> ".clod" </> "last-run"
        , timestamp = "20250401-000000"
        , currentStaging = outputDir
        , testMode = True
        , verbose = True  -- Set verbose to True for this example
        , ignorePatterns = []
        }
      
  putStrLn "=== Running Safe AI-Generated Function ==="
  
  -- Run the safe function
  result1 <- runClodM config $
    aiGeneratedFunctionWithOutput readCap writeCap targetFile outputDir
  
  case result1 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Safe function completed successfully"
  
  putStrLn "\n=== Running Malicious Function (should fail) ==="
  
  -- Run the malicious function
  result2 <- runClodM config $
    maliciousFunction readCap writeCap
  
  case result2 of
    Left err -> putStrLn $ "Correctly blocked malicious access: " ++ show err
    Right _ -> putStrLn "SECURITY FAILURE: Malicious function succeeded!"

-- This example demonstrates how the capability-based security system
-- prevents even malicious code from accessing files outside
-- specifically allowed directories.
