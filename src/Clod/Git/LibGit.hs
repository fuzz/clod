{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Clod.Git.LibGit
-- Description : Git operations using libgit2
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides Git operations using the libgit2 library through the 
-- hlibgit2 Haskell binding. It implements the core Git functionality needed by
-- Clod in a type-safe and efficient manner without relying on shell commands.
--
-- The module uses proper Haskell resource management for Git objects and provides
-- a strong typing discipline for Git operations.

module Clod.Git.LibGit
  ( -- * Repository operations
    getRepositoryRoot
  , isGitRepository
    
    -- * Status operations
  , checkUncommittedChanges
  , getModifiedFiles
  , getUntrackedFiles
  
    -- * Direct IO operations (for effects system)
  , directGetModifiedFiles
  , directGetUntrackedFiles
  ) where

import Control.Exception (try, bracket, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (newIORef, readIORef, modifyIORef')
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, castFunPtr)
import Foreign.Storable (peek)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

-- Import the hlibgit2 bindings
import qualified Bindings.Libgit2 as Git
import qualified Bindings.Libgit2.Status as GitStatus
import qualified Bindings.Libgit2.Repository as GitRepo

import Clod.Types (ClodM, ClodConfig(..), ClodError(..), throwError)

-- | Find the root directory of the current Git repository
-- Returns the absolute path to the repository root.
-- Throws a GitError if not in a Git repository.
getRepositoryRoot :: ClodM FilePath
getRepositoryRoot = do
  result <- liftIO $ try $ withRepository "." $ \repoPtr -> do
    -- Get the working directory path from the repository
    workdirPtr <- GitRepo.c'git_repository_workdir repoPtr
    if workdirPtr == nullPtr
      then do
        -- If workdir is null, use the repository path instead (bare repo)
        gitdirPtr <- GitRepo.c'git_repository_path repoPtr
        BC.unpack <$> BS.packCString gitdirPtr
      else 
        BC.unpack <$> BS.packCString workdirPtr
        
  case result of
    Left (e :: SomeException) -> throwError $ GitError $ "Failed to find git repository root: " ++ show e
    Right path -> return path

-- | Check if a directory is a Git repository
-- Returns Just root path if it is a repository, Nothing otherwise.
isGitRepository :: FilePath -> ClodM (Maybe FilePath)
isGitRepository path = do
  result <- liftIO $ try $ withRepository path $ \repoPtr -> do
    -- Get the working directory path from the repository
    workdirPtr <- GitRepo.c'git_repository_workdir repoPtr
    if workdirPtr == nullPtr
      then do
        -- If workdir is null, use the repository path instead (bare repo)
        gitdirPtr <- GitRepo.c'git_repository_path repoPtr
        BC.unpack <$> BS.packCString gitdirPtr
      else 
        BC.unpack <$> BS.packCString workdirPtr
        
  case result of
    Left (_ :: SomeException) -> return Nothing
    Right path' -> return $ Just path'

-- | Check if there are uncommitted changes in the repository
-- Returns True if there are uncommitted changes, False otherwise.
-- In interactive mode, prompts the user to continue if changes are found.
checkUncommittedChanges :: ClodConfig -> ClodM Bool
checkUncommittedChanges config = do
  repoPath <- getRepositoryRoot
  modifiedFiles <- getModifiedFiles repoPath
  untrackedFiles <- getUntrackedFiles repoPath
  
  let hasChanges = not (null modifiedFiles && null untrackedFiles)
  
  when hasChanges $ do
    liftIO $ putStrLn "Warning: You have uncommitted changes in your repository."
    liftIO $ putStrLn "It's recommended to commit your changes before running clod to ensure you can recover if needed."
    
    if testMode config
      then do
        liftIO $ putStrLn "Test mode: automatically continuing..."
        return ()
      else do
        liftIO $ putStr "Continue anyway? [y/N] "
        liftIO $ hFlush stdout
        response <- liftIO getChar
        liftIO $ putStrLn ""
        when (response `notElem` ['y', 'Y']) $ 
          throwError $ GitError "Operation cancelled by user"
  
  return hasChanges

-- | Get list of modified files in a Git repository
getModifiedFiles :: FilePath -> ClodM [FilePath]
getModifiedFiles repoPath = do
  -- Use git_status_foreach with a callback to collect modified files
  result <- liftIO $ try $ do
    withRepository repoPath $ \repoPtr -> do
      -- Create a list to store modified files
      modifiedFilesRef <- newIORef []
      
      -- Create callback function for status
      cb <- mkStatusCallback $ \pathPtr status _ -> do
        -- Check if file is modified in index or working tree
        let statusInt = fromIntegral status :: Int
            -- Use direct CInt conversions to avoid type-default warnings
            wt_modified = fromIntegral (GitStatus.c'GIT_STATUS_WT_MODIFIED :: CInt) :: Int
            index_modified = fromIntegral (GitStatus.c'GIT_STATUS_INDEX_MODIFIED :: CInt) :: Int
            modifiedFlag = wt_modified .|. index_modified
            isModified = (statusInt .&. modifiedFlag) /= 0
        
        when isModified $ do
          -- Get file path and add to the list
          path <- BC.unpack <$> BS.packCString pathPtr
          modifyIORef' modifiedFilesRef ((repoPath </> path) :)
        
        return 0
      
      -- Run the foreach function with our callback
      ret <- GitStatus.c'git_status_foreach repoPtr (castFunPtr cb) nullPtr
      when (ret < 0) $ error "Failed to get git status"
      
      -- Read the list of modified files
      readIORef modifiedFilesRef
  
  case result of
    Left (e :: SomeException) -> do
      liftIO $ putStrLn $ "Warning: Failed to get git modified files: " ++ show e
      return []
    Right files -> return files

-- | Get list of untracked files in a Git repository
getUntrackedFiles :: FilePath -> ClodM [FilePath]
getUntrackedFiles repoPath = do
  -- Use git_status_foreach with a callback to collect untracked files
  result <- liftIO $ try $ do
    withRepository repoPath $ \repoPtr -> do
      -- Create a list to store untracked files
      untrackedFilesRef <- newIORef []
      
      -- Create callback function for status
      cb <- mkStatusCallback $ \pathPtr status _ -> do
        -- Check if file is untracked
        let statusInt = fromIntegral status :: Int
            -- Use direct CInt conversion to avoid type-default warnings
            untrackedFlag = fromIntegral (GitStatus.c'GIT_STATUS_WT_NEW :: CInt) :: Int
            isUntracked = (statusInt .&. untrackedFlag) /= 0
        
        when isUntracked $ do
          -- Get file path and add to the list
          path <- BC.unpack <$> BS.packCString pathPtr
          modifyIORef' untrackedFilesRef ((repoPath </> path) :)
        
        return 0
      
      -- Run the foreach function with our callback
      ret <- GitStatus.c'git_status_foreach repoPtr (castFunPtr cb) nullPtr
      when (ret < 0) $ error "Failed to get git status"
      
      -- Read the list of untracked files
      readIORef untrackedFilesRef
  
  case result of
    Left (e :: SomeException) -> do
      liftIO $ putStrLn $ "Warning: Failed to get git untracked files: " ++ show e
      return []
    Right files -> return files

-- Status callback type and wrapper
type StatusCallback = CString -> CInt -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkStatusCallback :: StatusCallback -> IO (FunPtr StatusCallback)

-- | Direct IO version of getModifiedFiles for use with effects system
directGetModifiedFiles :: FilePath -> IO [FilePath]
directGetModifiedFiles repoPath = do
  -- Use git_status_foreach with a callback to collect modified files
  withRepository repoPath $ \repoPtr -> do
    -- Create a list to store modified files
    modifiedFilesRef <- newIORef []
    
    -- Create callback function for status
    cb <- mkStatusCallback $ \pathPtr status _ -> do
      -- Check if file is modified in index or working tree
      let statusInt = fromIntegral status :: Int
          -- Use direct CInt conversions to avoid type-default warnings
          wt_modified = fromIntegral (GitStatus.c'GIT_STATUS_WT_MODIFIED :: CInt) :: Int
          index_modified = fromIntegral (GitStatus.c'GIT_STATUS_INDEX_MODIFIED :: CInt) :: Int
          modifiedFlag = wt_modified .|. index_modified
          isModified = (statusInt .&. modifiedFlag) /= 0
      
      when isModified $ do
        -- Get file path and add to the list
        path <- BC.unpack <$> BS.packCString pathPtr
        modifyIORef' modifiedFilesRef ((repoPath </> path) :)
      
      return 0
    
    -- Run the foreach function with our callback
    ret <- GitStatus.c'git_status_foreach repoPtr (castFunPtr cb) nullPtr
    when (ret < 0) $ error "Failed to get git status"
    
    -- Read the list of modified files
    readIORef modifiedFilesRef

-- | Direct IO version of getUntrackedFiles for use with effects system
directGetUntrackedFiles :: FilePath -> IO [FilePath]
directGetUntrackedFiles repoPath = do
  -- Use git_status_foreach with a callback to collect untracked files
  withRepository repoPath $ \repoPtr -> do
    -- Create a list to store untracked files
    untrackedFilesRef <- newIORef []
    
    -- Create callback function for status
    cb <- mkStatusCallback $ \pathPtr status _ -> do
      -- Check if file is untracked
      let statusInt = fromIntegral status :: Int
          -- Use direct CInt conversion to avoid type-default warnings
          untrackedFlag = fromIntegral (GitStatus.c'GIT_STATUS_WT_NEW :: CInt) :: Int
          isUntracked = (statusInt .&. untrackedFlag) /= 0
      
      when isUntracked $ do
        -- Get file path and add to the list
        path <- BC.unpack <$> BS.packCString pathPtr
        modifyIORef' untrackedFilesRef ((repoPath </> path) :)
      
      return 0
    
    -- Run the foreach function with our callback
    ret <- GitStatus.c'git_status_foreach repoPtr (castFunPtr cb) nullPtr
    when (ret < 0) $ error "Failed to get git status"
    
    -- Read the list of untracked files
    readIORef untrackedFilesRef

-- | Helper function to open a repository and perform an action with it
withRepository :: FilePath -> (Ptr Git.C'git_repository -> IO a) -> IO a
withRepository path action = do
  -- Ensure libgit2 is properly initialized and cleaned up
  Git.withLibGitDo $ do
    -- Open the repository
    bracket 
      (openRepository path)  -- acquire resource
      GitRepo.c'git_repository_free  -- release resource
      action  -- use resource
  where
    openRepository :: FilePath -> IO (Ptr Git.C'git_repository)
    openRepository repoPath = alloca $ \repoPtr -> do
      -- Try to open the repository
      ret <- withCString repoPath $ \cPath ->
        GitRepo.c'git_repository_open repoPtr cPath
      
      -- Check for errors
      if ret < 0
        then do
          -- Get the last git error
          errPtr <- Git.c'giterr_last
          if errPtr == nullPtr
            then error "Failed to open git repository"
            else do
              -- Extract error message
              -- Git.c'git_error_message is not exported, use a generic error instead
              error "Failed to open git repository: libgit2 error"
        else
          -- Success - return the repository pointer
          peek repoPtr
