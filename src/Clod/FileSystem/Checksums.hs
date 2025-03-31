{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Clod.FileSystem.Checksums
-- Description : Checksums-based file tracking for Clod
-- Copyright   : (c) Fuzz Leonard, 2025
-- License     : MIT
-- Maintainer  : cyborg@bionicfuzz.com
-- Stability   : experimental
--
-- This module provides functions for tracking file changes using checksums.
-- It calculates SHA-256 hashes of file content and maintains a database of files
-- that have been processed, allowing us to detect new, modified, deleted, and renamed files.
--
-- The file checksum database is stored as a Dhall configuration file with the following structure:
--
-- @
-- { files =
--     { "path/to/file1.txt" =
--         { path = "path/to/file1.txt"
--         , checksum = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
--         , lastModified = "2025-01-01T12:00:00Z"
--         , optimizedName = "path-to-file1.txt"
--         }
--     , "path/to/file2.md" =
--         { path = "path/to/file2.md"
--         , checksum = "7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730"
--         , lastModified = "2025-01-02T14:30:00Z"
--         , optimizedName = "path-to-file2.md"
--         }
--     }
-- , checksums =
--     { "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" = "path/to/file1.txt"
--     , "7d865e959b2466918c9863afca942d0fb89d7c9ac0c99bafc3749504ded97730" = "path/to/file2.md"
--     }
-- , lastStagingDir = Some "./staging/20250101-120000"
-- , lastRunTime = "2025-01-01T12:00:00Z"
-- }
-- @
--
-- This database allows efficient lookup of files by path or checksum,
-- detection of renamed files (same content with different paths),
-- and tracking of previous staging directories.

module Clod.FileSystem.Checksums
  ( -- * Checksum operations
    calculateChecksum
  , checksumFile
    
    -- * Database operations
  , initializeDatabase
  , loadDatabase
  , saveDatabase
  , updateDatabase
  
    -- * Change detection
  , detectFileChanges
  , findChangedFiles
  , findRenamedFiles
  , getFileStatus
  , FileStatus(..)
  
    -- * Database management
  , cleanupStagingDirectories
  , flushMissingEntries
  ) where

import Control.Exception (try, IOException, SomeException)
import Control.Monad (when, forM)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (doesFileExist, doesDirectoryExist, getModificationTime, 
                         removeDirectoryRecursive, createDirectoryIfMissing, renameFile)
import System.FilePath ((</>), takeDirectory)
import GHC.Generics (Generic)
import Clod.Types
import Clod.FileSystem.Detection (safeFileExists, safeIsTextFile)
import Clod.FileSystem.Operations (safeReadFile)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.IO as TextIO
import qualified Dhall
import qualified Dhall.Core

-- User error helper for IOErrors
createError :: String -> IOError
createError = Prelude.userError

-- | Data type for tracking file status
data FileStatus
  = Unchanged     -- ^ File has not changed
  | New           -- ^ New file
  | Modified      -- ^ Existing file with modified content
  | Deleted       -- ^ File no longer exists
  | Renamed FilePath  -- ^ File was renamed (new path)
  deriving (Show, Eq, Generic)

-- | Calculate SHA-256 checksum of a ByteString
calculateChecksum :: BS.ByteString -> Checksum
calculateChecksum content =
  let hash = SHA256.hash content
      hexHash = Base16.encode hash
  in Checksum (show hexHash)

-- | Calculate the checksum of a file
-- Only text files are allowed to be checksummed
checksumFile :: FileReadCap -> FilePath -> ClodM Checksum
checksumFile readCap path = do
  -- Check if file exists
  fileExists <- safeFileExists readCap path
  if not fileExists
    then throwError $ FileSystemError path (createError "File does not exist")
    else do
      -- Check if it's a text file
      isText <- safeIsTextFile readCap path
      if not isText
        then throwError $ ChecksumError $ "Cannot checksum binary or ineligible file: " ++ path
        else do
          -- Read file content and calculate checksum
          content <- safeReadFile readCap path
          return $ calculateChecksum content

-- | Initialize a new, empty database
initializeDatabase :: ClodM ClodDatabase
initializeDatabase = do
  currentTime <- liftIO getCurrentTime
  return $ ClodDatabase
    { dbFiles = Map.empty
    , dbChecksums = Map.empty
    , dbLastStagingDir = Nothing
    , dbLastRunTime = currentTime
    }

-- | Load the database from disk using Dhall
loadDatabase :: FilePath -> ClodM ClodDatabase
loadDatabase dbPath = do
  -- Check if the database file exists
  fileExists <- liftIO $ doesFileExist dbPath
  if not fileExists
    then do
      -- If it doesn't exist, create a new database
      db <- initializeDatabase
      -- Ensure the directory exists and save
      liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
      saveDatabase dbPath db
      return db
    else do
      -- Try to parse the database file using Dhall
      eitherResult <- liftIO $ try @SomeException $ do
        -- Use Dhall.inputFile to correctly parse the database file
        sdb <- Dhall.inputFile Dhall.auto dbPath
        -- Convert to ClodDatabase and return
        return $ fromSerializable sdb
      
      case eitherResult of
        Right db -> return db
        Left err -> do
          -- If parsing fails, log the error in verbose mode
          config <- ask
          when (verbose config) $ 
            liftIO $ putStrLn $ "Warning: Failed to parse database: " ++ show err
          
          -- Create a new database
          whenVerbose $ liftIO $ putStrLn "Creating a new empty database"
          db <- initializeDatabase
          -- Save it right away to ensure it's in the right format for next time
          saveDatabase dbPath db
          return db
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action

-- | Save the database to disk using Dhall serialization
saveDatabase :: FilePath -> ClodDatabase -> ClodM ()
saveDatabase dbPath db = do
  -- Ensure the directory exists
  liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)
  
  -- Convert to serializable form 
  let serializedDb = toSerializable db
  
  -- Write to temporary file first to avoid locking issues
  let tempPath = dbPath ++ ".new"
  
  -- Use proper Dhall encoding
  eitherResult <- liftIO $ try @IOException $ do
    -- Use Dhall's encoding to create a properly formatted Dhall expression
    let dhallExpr = Dhall.embed Dhall.inject serializedDb
    let dhallText = Dhall.Core.pretty dhallExpr
    
    -- Write to the temp file
    TextIO.writeFile tempPath dhallText
    -- Then rename to actual path (atomic operation on most filesystems)
    renameFile tempPath dbPath
  
  case eitherResult of
    Left err -> throwError $ DatabaseError $ "Failed to save database: " ++ show err
    Right _ -> whenVerbose $ liftIO $ putStrLn $ "Successfully saved database to: " ++ dbPath
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action
      


-- | Update the database with a new file entry
updateDatabase :: ClodDatabase -> FilePath -> Checksum -> UTCTime -> OptimizedName -> ClodDatabase
updateDatabase db path checksum modTime optName =
  let 
    -- Create new file entry
    newEntry = FileEntry
      { entryPath = path
      , entryChecksum = checksum
      , entryLastModified = modTime
      , entryOptimizedName = optName
      }
    
    -- Update maps
    newFiles = Map.insert path newEntry (dbFiles db)
    newChecksums = Map.insert (unChecksum checksum) path (dbChecksums db)
  in
    db { dbFiles = newFiles, dbChecksums = newChecksums }

-- | Detect file status by comparing against database
getFileStatus :: ClodDatabase -> FilePath -> Checksum -> ClodM FileStatus
getFileStatus db path checksum = do
  let 
    files = dbFiles db
    checksums = dbChecksums db
    checksumStr = unChecksum checksum

  -- Check if file exists in database
  case Map.lookup path files of
    -- File doesn't exist in database
    Nothing -> 
      -- Check if file with same checksum exists (renamed file)
      case Map.lookup checksumStr checksums of
        Just oldPath -> 
          -- Only consider it renamed if the old path is different
          if oldPath /= path 
            then return $ Renamed oldPath
            else return New
        Nothing -> return New

    -- File exists in database
    Just entry ->
      -- Check if checksum matches
      if entryChecksum entry == checksum
        then return Unchanged
        else return Modified

-- | Find files that need processing (new, modified, renamed)
findChangedFiles :: ClodDatabase -> [(FilePath, Checksum, UTCTime)] -> ClodM [(FilePath, FileStatus)]
findChangedFiles db fileInfos = do
  whenVerbose $ liftIO $ putStrLn $ "Processing " ++ show (length fileInfos) ++ " files for change detection"
  
  -- Process each file
  forM fileInfos $ \(path, checksum, _) -> do
    status <- getFileStatus db path checksum
    whenVerbose $ liftIO $ putStrLn $ "File status for " ++ path ++ ": " ++ show status
    return (path, status)
  
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action

-- | Find files that have been renamed
findRenamedFiles :: ClodDatabase -> [(FilePath, FileStatus)] -> [(FilePath, FilePath)]
findRenamedFiles _ fileStatuses =
  mapMaybe extractRename fileStatuses
  where
    extractRename (newPath, Renamed oldPath) = Just (newPath, oldPath)
    extractRename _ = Nothing

-- | Detect changes by comparing current files with database
detectFileChanges :: FileReadCap -> ClodDatabase -> [FilePath] -> FilePath -> ClodM ([(FilePath, FileStatus)], [(FilePath, FilePath)])
detectFileChanges readCap db filePaths projectRoot = do
  whenVerbose $ liftIO $ putStrLn $ "Detecting changes for " ++ show (length filePaths) ++ " files"
  whenVerbose $ liftIO $ putStrLn $ "Database has " ++ show (Map.size (dbFiles db)) ++ " entries"
  
  -- For each file, calculate checksum and get modification time
  fileInfos <- catMaybes <$> forM filePaths (\path -> do
      let fullPath = projectRoot </> path
      
      -- Check if file exists
      fileExists <- safeFileExists readCap fullPath
      if not fileExists
        then do
          whenVerbose $ liftIO $ putStrLn $ "File does not exist: " ++ fullPath
          return Nothing
        else do
          -- Check if it's a text file
          isText <- safeIsTextFile readCap fullPath
          if not isText
            then do
              whenVerbose $ liftIO $ putStrLn $ "Not a text file: " ++ fullPath
              return Nothing
            else do
              -- Calculate checksum
              checksum <- checksumFile readCap fullPath
              -- Get modification time
              modTime <- liftIO $ getModificationTime fullPath
              whenVerbose $ liftIO $ putStrLn $ "Processed file: " ++ path ++ " with checksum: " ++ unChecksum checksum
              return $ Just (path, checksum, modTime)
    )
  
  -- Detect file statuses
  whenVerbose $ liftIO $ putStrLn $ "Got " ++ show (length fileInfos) ++ " files to check"
  changedFiles <- findChangedFiles db fileInfos
  
  -- Find renamed files
  let renamedFiles = findRenamedFiles db changedFiles
  whenVerbose $ liftIO $ putStrLn $ "Found " ++ show (length renamedFiles) ++ " renamed files"
  
  return (changedFiles, renamedFiles)
  
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action


-- | Clean up old staging directories
cleanupStagingDirectories :: ClodM ()
cleanupStagingDirectories = do
  config <- ask
  
  -- Check if there's a previous staging directory to clean up
  case previousStaging config of
    Nothing -> return ()
    Just oldDir -> do
      -- Check if directory exists
      dirExists <- liftIO $ doesDirectoryExist oldDir
      when dirExists $ do
        -- Remove the directory if it exists
        whenVerbose $ liftIO $ putStrLn $ "Cleaning up previous staging directory: " ++ oldDir
        result <- liftIO $ try $ removeDirectoryRecursive oldDir :: ClodM (Either IOException ())
        case result of
          Left err -> whenVerbose $ liftIO $ putStrLn $ "Warning: Failed to remove old staging directory: " ++ show err
          Right _ -> return ()
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action

-- | Find and remove missing files from the database
flushMissingEntries :: FileReadCap -> ClodDatabase -> FilePath -> ClodM ClodDatabase
flushMissingEntries readCap db projectRoot = do
  config <- ask
  
  -- Don't proceed unless in flush mode
  if not (flushMode config)
    then return db
    else do
      whenVerbose $ liftIO $ putStrLn "Checking for missing files to flush from database..."
      
      -- Check each file in database to see if it still exists
      let files = Map.toList (dbFiles db)
      existingEntries <- forM files $ \(path, entry) -> do
        let fullPath = projectRoot </> path
        fileExists <- safeFileExists readCap fullPath
        
        if fileExists
          then return (path, Just entry)
          else do
            whenVerbose $ liftIO $ putStrLn $ "File no longer exists: " ++ path
            return (path, Nothing)
      
      -- Filter out missing files
      let newFiles = Map.fromList [(path, entry) | (path, Just entry) <- existingEntries]
          missingCount = length (dbFiles db) - Map.size newFiles
          
      -- Rebuild checksums map
      let newChecksums = Map.fromList 
                       $ map (\entry -> (unChecksum (entryChecksum entry), entryPath entry)) 
                       $ Map.elems newFiles
      
      -- Report results
      whenVerbose $ liftIO $ putStrLn $ "Removed " ++ show missingCount ++ " missing files from database"
      
      -- Return updated database
      return db { dbFiles = newFiles, dbChecksums = newChecksums }
  where
    whenVerbose action = do
      config <- ask
      when (verbose config) action