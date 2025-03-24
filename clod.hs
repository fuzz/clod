{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Claude Git Project File Uploader
-- Tracks modified files in a git repo and prepares them for upload to Claude's Project Knowledge
-- Respects .gitignore patterns, skips package-lock.json and binary files

module Main where

import Control.Monad (filterM, forM, forM_, unless, when, void)
import Data.Aeson ((.=), object, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, IOError)
import System.Process
import qualified Data.List as L
import Control.Exception (try)
import Data.IORef

-- | Type for file operation results
data FileResult = Success | Skipped String
  deriving (Show, Eq)

-- | Configuration for the clod program
data ClodConfig = ClodConfig
  { projectPath :: FilePath      -- ^ Root path of the git repository
  , stagingDir :: FilePath       -- ^ Directory where files will be staged for Claude
  , configDir :: FilePath        -- ^ Directory for configuration files
  , lastRunFile :: FilePath      -- ^ File that marks when clod was last run
  , timestamp :: String          -- ^ Timestamp for the current run
  , currentStaging :: FilePath   -- ^ Path to the current staging directory
  , testMode :: Bool             -- ^ Whether we're running in test mode
  , ignorePatterns :: [String]   -- ^ Patterns from .clodignore
  }

-- | Main entry point
main :: IO ()
main = do
  -- Print version information
  putStrLn "clod version 1.1.0 (Haskell)"
  
  -- Check platform
  platform <- readProcess "uname" ["-s"] ""
  when (platform /= "Darwin\n") $ do
    putStrLn $ "Warning: clod is primarily designed for macOS. Some features may not work on " ++ platform
    putStrLn "Claude's filesystem access is currently only available on macOS and Windows desktop applications."
  
  -- Check for git dependency
  gitExists <- isJust <$> findExecutable "git"
  unless gitExists $ do
    putStrLn "Error: git is required but not installed or not in PATH"
    exitFailure
  
  -- Change to git repository root
  rootPath <- readProcess "git" ["rev-parse", "--show-toplevel"] ""
  let projectPath = init rootPath  -- remove trailing newline
  setCurrentDirectory projectPath
  
  -- Check for uncommitted changes
  uncommittedChanges <- readProcess "git" ["status", "--porcelain"] ""
  when (uncommittedChanges /= "") $ do
    putStrLn "Warning: You have uncommitted changes in your repository."
    putStrLn "It's recommended to commit your changes before running clod to ensure you can recover if needed."
    
    -- In test mode, automatically proceed with "Y"
    isTestMode <- isJust <$> lookupEnv "CLOD_TEST_MODE"
    if isTestMode
      then putStrLn "Test mode: automatically continuing..."
      else do
        putStr "Continue anyway? [y/N] "
        hFlush stdout
        response <- getChar
        putStrLn ""
        unless (response `elem` ['y', 'Y']) exitFailure
  
  -- Allow user to configure staging directory
  homeDir <- getHomeDirectory
  let defaultStagingDir = homeDir </> "Claude"
  
  -- Get staging directory (from env var in test mode, or prompt)
  isTestMode <- isJust <$> lookupEnv "CLOD_TEST_MODE"
  testStagingDir <- lookupEnv "CLOD_TEST_STAGING_DIR"
  
  stagingDir <- if isTestMode
                then return $ fromMaybe defaultStagingDir testStagingDir
                else do
                  putStr $ "Staging directory [" ++ defaultStagingDir ++ "]: "
                  hFlush stdout
                  response <- getLine
                  return $ if null response then defaultStagingDir else response
  
  -- Config files - store in the git repo under .claude-uploader
  let configDir = projectPath </> ".claude-uploader"
      lastRunFile = configDir </> "last-run-marker"
  
  -- Create config directory if it doesn't exist
  createDirectoryIfMissing True configDir
  createDirectoryIfMissing True stagingDir
  
  -- Create timestamp directory for this run
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now
      currentStaging = stagingDir </> ("ClaudeUpload_" ++ timestamp)
  
  createDirectoryIfMissing True currentStaging
  
  -- Read .clodignore file if it exists
  ignorePatterns <- readClodIgnore projectPath
  when (not (null ignorePatterns)) $ do
    putStrLn $ "Found .clodignore with " ++ show (length ignorePatterns) ++ " patterns"
  
  putStrLn $ "Looking for modified files in " ++ projectPath ++ "..."
  
  -- Initialize path manifest
  let manifestPath = currentStaging </> "_path_manifest.json"
  writeFile manifestPath "{\n"
  
  -- Create the configuration record
  let config = ClodConfig
        { projectPath = projectPath
        , stagingDir = stagingDir
        , configDir = configDir
        , lastRunFile = lastRunFile
        , timestamp = timestamp
        , currentStaging = currentStaging
        , testMode = isTestMode
        , ignorePatterns = ignorePatterns
        }
  
  -- Process files based on whether this is the first run
  (fileCount, skippedCount) <- do
    lastRunExists <- doesFileExist lastRunFile
    if lastRunExists
      then do
        putStrLn "Finding files modified since last run..."
        processModifiedFiles config manifestPath
      else do
        putStrLn "First run - no previous timestamp found."
        
        -- In test mode, automatically choose option 'a'
        importOption <- if isTestMode
                        then do
                          putStrLn "Test mode: automatically importing all files"
                          return 'a'
                        else do
                          putStrLn "Options:"
                          putStrLn "  a: Import all files (respecting .gitignore)"
                          putStrLn "  m: Import only modified files (git status)"
                          putStrLn "  n: Import nothing (just set timestamp)"
                          putStr "Choose an option [a/m/n]: "
                          hFlush stdout
                          opt <- getLine
                          return $ if null opt then 'n' else head opt
        
        case importOption of
          'a' -> do
            putStrLn "Importing all files (respecting .gitignore)..."
            processAllFiles config manifestPath
          'm' -> do
            putStrLn "Importing modified files from git status..."
            processModifiedFiles config manifestPath
          _   -> do 
            putStrLn "Setting timestamp only."
            return (0, 0)
  
  -- Close the path manifest JSON
  appendFile manifestPath "\n}"
  
  -- Update the last run marker
  writeFile lastRunFile ""
  
  -- Handle results
  if fileCount == 0
    then do
      putStrLn $ "No files processed (skipped: " ++ show skippedCount ++ ")."
      -- Clean up the empty staging directory (just the manifest file)
      safeRemoveFile manifestPath
      exitSuccess
    else do
      -- Open the staging directory (skip in test mode)
      unless isTestMode $ do
        case platform of
          "Darwin\n" -> callProcess "open" [currentStaging]
          _          -> putStrLn $ "Staging directory: " ++ currentStaging
      
      putStrLn $ "Success! " ++ show fileCount ++ " files prepared for upload. Skipped: " ++ show skippedCount
      putStrLn $ "Staging directory: " ++ currentStaging
      
      -- Only show next steps if not in test mode
      unless isTestMode $ do
        putStrLn ""
        putStrLn "Next steps:"
        putStrLn "1. Navigate to Project Knowledge in your Claude Project (Pro or Team account required)"
        putStrLn "2. Drag files from the staging folder to Project Knowledge"
        putStrLn "3. Don't forget _path_manifest.json which maps optimized names back to original paths"
        putStrLn "4. Paste the contents of project-instructions.md into the Project Instructions section"
        putStrLn "5. IMPORTANT: You must manually delete previous versions of these files from Project Knowledge"
        putStrLn "   before starting a new conversation to ensure Claude uses the most recent files"
        putStrLn "6. Start a new conversation to see changes"

-- | Safely remove a file, ignoring errors if it doesn't exist
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile path = do
  exists <- doesFileExist path
  when exists $ removeFile path

-- | Read and parse .clodignore file
readClodIgnore :: FilePath -> IO [String]
readClodIgnore projectPath = do
  let ignorePath = projectPath </> ".clodignore"
  doesFileExist ignorePath >>= \exists -> 
    if exists
      then filter isValidPattern . lines <$> readFile ignorePath
      else return []
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)

-- | Check if a file matches any ignore pattern
matchesIgnorePattern :: [String] -> FilePath -> Bool
matchesIgnorePattern patterns filePath =
  any matchPattern patterns
  where
    matchPattern pattern
      -- File extension pattern: *.ext
      | "*." `L.isPrefixOf` pattern = drop 1 pattern `L.isSuffixOf` filePath
      -- Directory/path pattern (contains slash)
      | '/' `elem` pattern = 
          let normalizedPattern = if last pattern == '/' then pattern else pattern ++ "/"
              normalizedPath = filePath ++ "/"
          in normalizedPattern `L.isPrefixOf` normalizedPath
      -- Simple filename or pattern with no slashes
      | otherwise = pattern == takeFileName filePath

-- | Process all files tracked by git or untracked but not ignored
processAllFiles :: ClodConfig -> FilePath -> IO (Int, Int)
processAllFiles config manifestPath = do
  -- Get both tracked and untracked files
  trackedFiles <- lines <$> readProcess "git" ["ls-files"] ""
  untrackedFiles <- lines <$> readProcess "git" ["ls-files", "--others", "--exclude-standard"] ""
  
  -- Process all files
  let allFiles = L.nub (trackedFiles ++ untrackedFiles)
  processFiles config manifestPath allFiles

-- | Process only modified files
processModifiedFiles :: ClodConfig -> FilePath -> IO (Int, Int)
processModifiedFiles config manifestPath = do
  -- Get all modified files (combined command)
  modifiedOutput <- readProcess "git" ["ls-files", "--modified", "--others", "--exclude-standard"] ""
  diffOutput <- readProcess "git" ["diff", "--name-only"] ""
  stagedOutput <- readProcess "git" ["diff", "--staged", "--name-only"] ""
  
  let modifiedFiles = lines modifiedOutput
      diffFiles = lines diffOutput
      stagedFiles = lines stagedOutput
      allFiles = L.nub (modifiedFiles ++ diffFiles ++ stagedFiles)
  
  processFiles config manifestPath allFiles

-- | Process a list of files
processFiles :: ClodConfig -> FilePath -> [FilePath] -> IO (Int, Int)
processFiles config manifestPath files = do
  -- Track if the current entry is the first in the manifest
  ref <- newIORef True
  
  -- Process files and count results
  results <- forM files $ \file -> do
    -- Get full path
    let fullPath = projectPath config </> file
    
    -- Skip if not a regular file
    isFile <- doesFileExist fullPath
    if not isFile
      then return (0, 0)
      else do
        -- Skip any files in the staging directory
        if stagingDir config `L.isInfixOf` fullPath
          then do
            putStrLn $ "Skipping: " ++ fullPath ++ " (in staging directory)"
            return (0, 0)
          else do
            -- Try to process the file
            result <- processFile config manifestPath fullPath file ref
            case result of
              Success -> return (1, 0)
              Skipped reason -> do
                putStrLn $ "Skipping: " ++ file ++ " (" ++ reason ++ ")"
                return (0, 1)
  
  -- Sum the file and skipped counts
  return (sum (map fst results), sum (map snd results))

-- | Process a single file
processFile :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> IO FileResult
processFile config manifestPath fullPath relPath firstEntryRef = do
  -- Skip specifically excluded files
  if relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"]
    then return $ Skipped "excluded file"
    else do
      -- Check if file should be ignored according to .clodignore patterns
      let ignorePatterns' = ignorePatterns config
      
      if not (null ignorePatterns') && matchesIgnorePattern ignorePatterns' relPath
        then return $ Skipped "matched .clodignore pattern"
        else do
          -- Skip binary files
          isText <- isTextFile fullPath
          if not isText
            then return $ Skipped "binary file"
            else do
              -- Create optimized filename
              let dirPart = takeDirectory relPath
                  fileName = takeFileName relPath
                  
                  -- Create the optimized name by replacing slashes with dashes
                  optimizedName = if dirPart /= "."
                                  then map (\c -> if c == '/' then '-' else c) dirPart ++ "-" ++ fileName
                                  else fileName
                  
                  -- Handle SVG files specially - change to XML extension for Claude compatibility
                  finalOptimizedName = if ".svg" `L.isSuffixOf` fileName
                                      then take (length optimizedName - 4) optimizedName ++ "-svg.xml"
                                      else optimizedName
              
              -- Copy file with optimized name
              copyFile fullPath (currentStaging config </> finalOptimizedName)
              
              -- Add to path manifest - use firstEntryRef to track whether a comma is needed
              isFirst <- readIORef firstEntryRef
              unless isFirst $
                appendFile manifestPath ",\n"
              writeIORef firstEntryRef False
              
              -- Escape JSON special characters
              let escapedOptimizedName = escapeJSON finalOptimizedName
                  escapedRelPath = escapeJSON relPath
                  manifestEntry = "  \"" ++ escapedOptimizedName ++ "\": \"" ++ escapedRelPath ++ "\""
              
              appendFile manifestPath manifestEntry
              
              putStrLn $ "Copied: " ++ relPath ++ " → " ++ finalOptimizedName
              return Success

-- | Check if a file is a text file using the 'file' command or extension
isTextFile :: FilePath -> IO Bool
isTextFile file = do
  -- Use 'file' command to check mime type
  result <- try (readProcess "file" ["--mime-type", "-b", file] "") :: IO (Either IOError String)
  case result of
    Left _ -> checkByExtension file  -- If command fails, fall back to extension
    Right mimeType -> 
      if "text/" `L.isPrefixOf` mimeType
        then return True
        else checkByExtension file  -- If not text mime type, check extension

-- | Check if a file is likely a text file based on its extension
checkByExtension :: FilePath -> IO Bool
checkByExtension file = do
  let ext = takeExtension file
      textExtensions = [".md", ".txt", ".js", ".jsx", ".ts", ".tsx", ".html", ".css", 
                       ".scss", ".json", ".yaml", ".yml", ".xml", ".svg", ".sh", ".py", 
                       ".rb", ".php", ".hs", ".cabal", ".h", ".c", ".cpp", ".java"]
  return $ ext `elem` textExtensions

-- | Escape JSON special characters 
escapeJSON :: String -> String
escapeJSON = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]