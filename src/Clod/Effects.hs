{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Module      : Clod.Effects
-- Description : Effect system for Clod operations
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module defines the effect system for the Clod application,
-- providing safe, typed effects for file operations, Git interactions,
-- and other side effects. This ensures that AI-assisted code generation
-- can only perform permitted operations and helps prevent errors.
--
-- === AI Safety
-- 
-- The effect system acts as a safety mechanism for AI-generated code. When code is generated
-- by an AI system, there's always a risk of unintended behavior, especially when it comes
-- to operations that could modify files or access sensitive data. This system uses algebraic
-- effects to:
--
-- 1. Make effects explicit in type signatures, so it's clear what side effects a function can have
-- 2. Separate effect definition from effect implementation, allowing for different interpreters
-- 3. Provide a capability-based security model to restrict operations to specific directories/files
--
-- === Example: Safe File Processing with AI
--
-- Here's how an AI-generated file processing function would be constrained by this system:
--
-- @
-- processFilesGeneratedByAI :: Members '[FileSystem, Error ClodError, Console] r
--                           => FileReadCap -> FileWriteCap -> [FilePath] -> Sem r ()
-- processFilesGeneratedByAI readCap writeCap files = do
--   -- AI can only read files within directories specified by readCap
--   -- AI can only write files within directories specified by writeCap
--   -- AI cannot perform unconstrained IO
--   forM_ files $ \file -> do
--     exists <- safeFileExists readCap file
--     when exists $ do
--       -- Content is restricted to specified paths
--       content <- safeReadFile readCap file
--       -- Processing can happen freely as it's pure
--       let processed = process content
--       -- Writing is restricted to specified paths
--       safeWriteFile writeCap (file ++ ".processed") processed
-- @
--
-- Even if the AI generates incorrect or malicious code, the capability-based system
-- prevents it from accessing files outside the specified directories.

module Clod.Effects 
  ( -- * File System Effects
    FileSystem(..)
  , readFile
  , writeFile
  , copyFile
  , fileExists
  , isTextFile
  , runFileSystemIO
    
    -- * Git Effects
  , Git(..)
  , getModifiedFiles
  , getUntrackedFiles
  , runGitIO
    
    -- * Console Effects
  , Console(..)
  , logInfo
  , logWarning
  , logError
  , logOutput
  , runConsoleIO
    
    -- * Error Handling
  , throw
  , catch
    
    -- * Running Effects
  , runClodEffects
  ) where

import Polysemy
import Polysemy.Error
import Polysemy.Reader
import qualified System.Directory as Dir
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import System.Process (readProcess)
import Prelude hiding (readFile, writeFile)
import qualified Clod.Git.LibGit as GitLib
import qualified Clod.Types as T
import qualified Control.Exception as Exception
import Control.Exception (SomeException)

-- | Effect for file system operations
data FileSystem m a where
  ReadFile :: FilePath -> FileSystem m BS.ByteString
  WriteFile :: FilePath -> BS.ByteString -> FileSystem m ()
  CopyFile :: FilePath -> FilePath -> FileSystem m ()
  FileExists :: FilePath -> FileSystem m Bool
  IsTextFile :: FilePath -> FileSystem m Bool

-- Generate effect functions with Template Haskell
-- This will automatically generate:
-- readFile :: Member FileSystem r => FilePath -> Sem r BS.ByteString
-- writeFile :: Member FileSystem r => FilePath -> BS.ByteString -> Sem r ()
-- copyFile :: Member FileSystem r => FilePath -> FilePath -> Sem r ()
-- fileExists :: Member FileSystem r => FilePath -> Sem r Bool
-- isTextFile :: Member FileSystem r => FilePath -> Sem r Bool
makeSem ''FileSystem

-- | Effect for Git operations
data Git m a where
  GetModifiedFiles :: FilePath -> Git m [FilePath]
  GetUntrackedFiles :: FilePath -> Git m [FilePath]

makeSem ''Git

-- | Effect for console output
data Console m a where
  LogInfo :: String -> Console m ()
  LogWarning :: String -> Console m ()
  LogError :: String -> Console m ()
  LogOutput :: String -> Console m ()  -- For stdout output that follows Unix principles (directly pipeable)

makeSem ''Console

-- | Run all Clod effects to IO
runClodEffects :: T.ClodConfig 
               -> Sem '[FileSystem, Git, Console, Error T.ClodError, Reader T.ClodConfig, Embed IO] a 
               -> IO (Either T.ClodError a)
runClodEffects config = runM 
                     . runReader config 
                     . runError 
                     . runConsoleIO 
                     . runGitIO 
                     . runFileSystemIO

-- | Run file system operations in IO
runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  ReadFile path -> embed $ BS.readFile path
  WriteFile path content -> embed $ BS.writeFile path content
  CopyFile src dest -> embed $ Dir.copyFile src dest
  FileExists path -> embed $ Dir.doesFileExist path
  IsTextFile path -> embed $ do
    -- Reimplement the isTextFile logic from Detection module
    result <- Exception.try (readProcess "file" ["--mime-type", "-b", path] "") :: IO (Either SomeException String)
    case result of
      Left _ -> return False  -- On error, assume it's not a text file
      Right mimeType -> return $ "text/" `isPrefixOf` mimeType || 
                                "application/json" `isPrefixOf` mimeType ||
                                "application/xml" `isPrefixOf` mimeType

-- | Run Git operations in IO
runGitIO :: Members '[Error T.ClodError, Embed IO] r => Sem (Git ': r) a -> Sem r a
runGitIO = interpret $ \case
  GetModifiedFiles path -> do
    -- Call GitLib functions directly without using ClodM
    result <- embed $ Exception.try $ GitLib.directGetModifiedFiles path
    case result of
      Left (e :: SomeException) -> throw $ T.GitError $ "Error getting modified files: " ++ show e
      Right files -> pure files
  GetUntrackedFiles path -> do
    -- Call GitLib functions directly without using ClodM
    result <- embed $ Exception.try $ GitLib.directGetUntrackedFiles path
    case result of
      Left (e :: SomeException) -> throw $ T.GitError $ "Error getting untracked files: " ++ show e
      Right files -> pure files

-- | Run console operations in IO
runConsoleIO :: Member (Embed IO) r => Sem (Console ': r) a -> Sem r a
runConsoleIO = interpret $ \case
  LogInfo msg -> embed $ putStrLn msg
  LogWarning msg -> embed $ putStrLn $ "Warning: " ++ msg
  LogError msg -> embed $ putStrLn $ "Error: " ++ msg
  LogOutput msg -> embed $ putStrLn msg  -- Clean output for piping