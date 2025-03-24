{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Claude Git Project File Uploader
-- Tracks modified files in a git repo and prepares them for upload to Claude's Project Knowledge
-- Respects .gitignore patterns, skips package-lock.json and binary files

module Main where

import Control.Monad (filterM, forM_, unless, when)
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
import System.IO.Error (catchIOError)
import System.Process
import Data.IORef
import Data.Char (toLower)
import qualified Data.List as L

-- | Type for file operation results
-- This is a simple sum type that allows us to track whether operations succeeded
data FileResult = Success | Skipped String
  deriving (Show, Eq)

-- | Configuration for the clod program
-- Haskell's record syntax gives us a clear way to group related data
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
  -- Create refs to track file counts
  fileCountRef <- newIORef 0
  skippedCountRef <- newIORef 0
  
  lastRunExists <- doesFileExist lastRunFile
  if lastRunExists
    then do
      putStrLn "Finding files modified since last run..."
      processModifiedFiles config manifestPath fileCountRef skippedCountRef
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
          processAllFiles config manifestPath fileCountRef skippedCountRef
        'm' -> do
          putStrLn "Importing modified files from git status..."
          processModifiedFiles config manifestPath fileCountRef skippedCountRef
        _   -> putStrLn "Setting timestamp only."
  
  -- Close the path manifest JSON
  appendFile manifestPath "\n}"
  
  -- Update the last run marker
  writeFile lastRunFile ""
  
  -- Get final file counts
  fileCount <- readIORef fileCountRef
  skippedCount <- readIORef skippedCountRef
  
  if fileCount == 0
    then do
      putStrLn $ "No files processed (skipped: " ++ show skippedCount ++ ")."
      -- Clean up the empty staging directory
      removeFile manifestPath
      removeDirectory currentStaging
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

-- | Read and parse .clodignore file
readClodIgnore :: FilePath -> IO [String]
readClodIgnore projectPath = do
  let ignorePath = projectPath </> ".clodignore"
  exists <- doesFileExist ignorePath
  if exists
    then do
      content <- readFile ignorePath
      let patterns = filter isValidPattern $ lines content
      return patterns
    else return []
  where
    isValidPattern line = not (null line) && not ("#" `L.isPrefixOf` line)

-- | Check if a file matches any ignore pattern
matchesIgnorePattern :: [String] -> FilePath -> Bool
matchesIgnorePattern patterns filePath =
  any (\pattern -> pattern `isInfixOf` filePath || filePath `isInfixOf` pattern) patterns

-- | Process all files tracked by git or untracked but not ignored
processAllFiles :: ClodConfig -> FilePath -> IORef Int -> IORef Int -> IO ()
processAllFiles config manifestPath fileCountRef skippedCountRef = do
  -- Get all files tracked by git
  trackedFiles <- lines <$> readProcess "git" ["ls-files"] ""
  
  -- Get all untracked but not ignored files
  untrackedFiles <- lines <$> readProcess "git" ["ls-files", "--others", "--exclude-standard"] ""
  
  -- Process all files
  firstEntryRef <- newIORef True
  let allFiles = nub (trackedFiles ++ untrackedFiles) -- nub removes duplicates
  
  forM_ allFiles $ \file -> do
    -- Get full path
    let fullPath = projectPath config </> file
    
    -- Skip if not a regular file
    isFile <- doesFileExist fullPath
    unless isFile $ return ()
    
    -- Skip any files in the staging directory
    when (stagingDir config `isInfixOf` fullPath) $ do
      putStrLn $ "Skipping: " ++ fullPath ++ " (in staging directory)"
      return ()
    
    -- Try to process the file
    result <- processFile config manifestPath fullPath file firstEntryRef
    case result of
      Success -> modifyIORef fileCountRef (+1)
      Skipped reason -> do
        putStrLn $ "Skipping: " ++ file ++ " (" ++ reason ++ ")"
        modifyIORef skippedCountRef (+1)

-- | Process only modified files
processModifiedFiles :: ClodConfig -> FilePath -> IORef Int -> IORef Int -> IO ()
processModifiedFiles config manifestPath fileCountRef skippedCountRef = do
  -- Get modified files
  modifiedFiles <- lines <$> readProcess "git" ["ls-files", "--modified"] ""
  
  -- Get files that have differences
  diffFiles <- lines <$> readProcess "git" ["diff", "--name-only"] ""
  
  -- Get untracked files
  untrackedFiles <- lines <$> readProcess "git" ["ls-files", "--others", "--exclude-standard"] ""
  
  -- Get staged files (new or modified)
  stagedFiles <- lines <$> readProcess "git" ["diff", "--staged", "--name-only"] ""
  
  -- Get newly added files since last commit
  newFiles <- lines <$> readProcess "git" ["ls-files", "--others", "--exclude-standard"] ""
  
  -- Process the files
  firstEntryRef <- newIORef True
  let allFiles = nub (modifiedFiles ++ diffFiles ++ untrackedFiles ++ stagedFiles ++ newFiles)
  
  forM_ allFiles $ \file -> do
    -- Get full path
    let fullPath = projectPath config </> file
    
    -- Skip if not a regular file
    isFile <- doesFileExist fullPath
    unless isFile $ return ()
    
    -- Skip any files in the staging directory
    when (stagingDir config `isInfixOf` fullPath) $ do
      putStrLn $ "Skipping: " ++ fullPath ++ " (in staging directory)"
      return ()
    
    -- Try to process the file
    result <- processFile config manifestPath fullPath file firstEntryRef
    case result of
      Success -> modifyIORef fileCountRef (+1)
      Skipped reason -> do
        putStrLn $ "Skipping: " ++ file ++ " (" ++ reason ++ ")"
        modifyIORef skippedCountRef (+1)

-- | Process a single file
-- Here we use pattern matching and early returns to handle special cases
processFile :: ClodConfig -> FilePath -> FilePath -> FilePath -> IORef Bool -> IO FileResult
processFile config manifestPath fullPath relPath firstEntryRef = do
  -- Skip specifically excluded files
  if relPath `elem` [".gitignore", "package-lock.json", "yarn.lock", ".clodignore"]
    then return $ Skipped "excluded file"
    else do
      -- Skip files matching .clodignore patterns
      if matchesIgnorePattern (ignorePatterns config) relPath
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
  result <- try $ readProcess "file" ["--mime-type", "-b", file] ""
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

-- | Check if a string is a substring of another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (Main.isPrefixOf needle) (tails haystack)

-- | Check if a string is a prefix of another
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = (x == y) && Main.isPrefixOf xs ys

-- | Check if a string is a suffix of another
isSuffixOf :: String -> String -> Bool
isSuffixOf x y = reverse x `Main.isPrefixOf` reverse y

-- | Get all tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-- | Get unique elements from a list
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- | Try executing an IO action, returning Left e on exception
try :: IO a -> IO (Either IOError a)
try action = (Right <$> action) `catch` (return . Left)

-- | Catch an exception
catch :: IO a -> (IOError -> IO a) -> IO a
catch = catchIOError
