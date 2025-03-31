# Clod Project Architecture

This document outlines the architecture, components, and design decisions for the Clod project.

## Project Overview

Clod (Claude Optimization Driver) is a tool designed to prepare text files for optimal processing by Claude AI models. It helps:

- Transform text files to be more Claude-friendly
- Generate manifest files for mapping between original and transformed paths
- Create staging directories with optimized files
- Skip binary files automatically
- Respect ignore patterns (.gitignore, .clodignore)
- Detect file changes efficiently

## Key Components

### Module Structure

- `Clod.Types`: Core data types and type classes
- `Clod.Core`: Main application logic
- `Clod.FileSystem`: File system operations
  - `Clod.FileSystem.Checksums`: Checksum-based file tracking
  - `Clod.FileSystem.Detection`: File type detection
  - `Clod.FileSystem.Operations`: File operations
  - `Clod.FileSystem.Processing`: File processing
  - `Clod.FileSystem.Transformations`: Text transformations
- `Clod.Output`: Output formatting and display
- `Clod.IgnorePatterns`: Pattern matching for ignored files
- `Clod.Capability`: Capability-based security
- `Clod.Config`: Configuration management

## Checksum-Based File Tracking

Clod uses a checksum-based approach for tracking file changes:

- Checksums detect file content changes regardless of modification time
- Works in any directory structure without requiring a Git repository
- Detects file renames by matching identical content with different paths
- Provides a consistent experience across different environments

```haskell
-- Calculate SHA-256 checksum of a ByteString
calculateChecksum :: BS.ByteString -> Checksum
calculateChecksum content =
  let hash = SHA256.hash content
      hexHash = Base16.encode hash
  in Checksum (show hexHash)
```

## Database Structure

The database structure enables efficient queries:

- `dbFiles` maps file paths to their entries for quick lookup by path
- `dbChecksums` maps checksums to paths for detecting renamed files
- `dbLastStagingDir` stores the previous staging directory for the `--last` flag
- Entries contain the full file metadata including content hash, modification time, and optimized name

```haskell
data ClodDatabase = ClodDatabase
  { dbFiles :: !(Map.Map FilePath FileEntry)
  , dbChecksums :: !(Map.Map String FilePath)
  , dbLastStagingDir :: !(Maybe FilePath)
  , dbLastRunTime :: !UTCTime
  } deriving stock (Show, Eq)
```

## Command-Line Interface

Clod provides several command-line options:

- `--verbose`: Enable verbose output
- `--staging`: Specify a custom staging directory
- `--flush`: Flush stale entries from the database
- `--last`: Use the previous staging directory
- `--all-files`: Process all files, not just new or modified ones
- `--modified`: Process only modified files
- `--quiet`: Suppress non-essential output
- `--version`: Show version information
- `--help`: Show help text

## File Change Detection

Clod identifies the following file statuses:

1. `New`: Files not previously in the database
2. `Modified`: Files with a different checksum than the database entry
3. `Unchanged`: Files with the same checksum as the database entry
4. `Deleted`: Files that were in the database but don't exist anymore
5. `Renamed`: Files with an identical checksum but different path

```haskell
detectFileChanges :: FileReadCap -> ClodDatabase -> [FilePath] -> FilePath -> ClodM ([(FilePath, FileStatus)], [(FilePath, FilePath)])
detectFileChanges readCap db filePaths projectRoot = do
  -- For each file, calculate checksum and get modification time
  fileInfos <- catMaybes <$> forM filePaths (\path -> do
      -- Check if file exists and is text, then calculate checksum
      -- ...
    )
  
  -- Detect file statuses
  changedFiles <- findChangedFiles db fileInfos
  
  -- Find renamed files
  let renamedFiles = findRenamedFiles db changedFiles
  
  return (changedFiles, renamedFiles)
```

## Binary File Detection

Clod employs the `magic` library for reliable binary file detection:

1. Uses the same detection mechanism as the Unix `file` command
2. Identifies files by examining their content signatures
3. Provides accurate MIME type classification
4. Works consistently across different platforms

```haskell
-- Check if a file is a text file using libmagic
safeIsTextFile :: FileReadCap -> FilePath -> ClodM Bool
safeIsTextFile readCap path = do
  -- Verify file is within allowed directories first
  allowed <- liftIO $ isPathAllowed (allowedReadDirs readCap) path
  if not allowed
    then throwError $ CapabilityError $ 
      "Access denied: Cannot read file outside allowed directories: " ++ path
    else do
      -- Get the MIME type from the magic library
      mimeType <- liftIO $ getMimeType path
      -- Check if it's a text MIME type
      return $ isMimeTypeText mimeType
```

## Capability-Based Security

Clod implements capability-based security for file operations:

- `FileReadCap`: Grants permission to read from specific directories
- `FileWriteCap`: Grants permission to write to specific directories
- `FileTransformCap`: Grants permission to transform file contents

Each capability checks paths against allowed directories before performing operations.

## Manifest Generation

Clod generates manifest files that map transformed file paths to their original locations:

```json
{
  "transformed/file.txt": "/original/path/to/file.txt",
  "transformed/another-file.txt": "/original/path/to/another-file.txt"
}
```

The manifest helps reconstruct file origins after processing.

## Staging Directory Management

Clod creates staging directories with the following structure:

```
staging-dir/
├── file1.txt
├── file2.txt
├── _path_manifest.dhall
└── ...
```

The `--last` flag functionality allows using the previous staging directory:

```haskell
-- Part of the mainLogic function
when lastMode $ do
  case dbLastStagingDir database of
    Just prevStaging -> do
      when verbose $ liftIO $ hPutStrLn stderr $ 
        "Using previous staging directory: " ++ prevStaging
      -- Output the previous staging directory and exit
      liftIO $ hPutStrLn stdout prevStaging
      throwError $ ConfigError "Using last staging directory as requested"
        
    Nothing -> do
      when verbose $ liftIO $ hPutStrLn stderr 
        "No previous staging directory available, proceeding with new staging"
```

## Error Handling

Clod uses a monad transformer stack for comprehensive error handling:

```haskell
-- Type for the Clod monad
type ClodM a = ExceptT ClodError IO a

-- Error types
data ClodError
  = FileSystemError FilePath IOError
  | ChecksumError String
  | DatabaseError String
  | ConfigError String
  | CapabilityError String
  | IgnorePatternError String
  deriving (Show, Eq)

-- Running the Clod monad
runClodM :: ClodM a -> IO (Either ClodError a)
runClodM = runExceptT
```

## Ignore Pattern Handling

Clod processes ignore patterns similar to .gitignore:

```haskell
-- Parse ignore patterns from file
parseIgnorePatterns :: FilePath -> ClodM [IgnorePattern]
parseIgnorePatterns path = do
  exists <- liftIO $ doesFileExist path
  if not exists
    then return []
    else do
      content <- liftIO $ readFile path
      return $ map IgnorePattern $ parseLines content
  where
    parseLines = filter (not . ignoreLine) . lines
    ignoreLine line = null line || "#" `isPrefixOf` line

-- Check if a file matches ignore patterns
matchesIgnorePattern :: [IgnorePattern] -> FilePath -> Bool
matchesIgnorePattern patterns path =
  any (\(IgnorePattern pattern) -> pathMatchesPattern pattern path) patterns
```

## File Processing Pipeline

The main file processing pipeline consists of several steps:

1. **File Discovery**: Find all files in the project directory
2. **Change Detection**: Identify new, modified, and unchanged files
3. **Filtering**: Apply ignore patterns to exclude files
4. **Text Detection**: Skip binary files
5. **Transformation**: Apply transformations to text files
6. **Staging**: Copy transformed files to the staging directory
7. **Manifest Generation**: Create a mapping between original and transformed paths
8. **Database Update**: Store file metadata for future runs

```haskell
-- Main processing logic
processFiles :: FileReadCap -> FileWriteCap -> FileTransformCap -> [FilePath] -> ClodM ProcessingStats
processFiles readCap writeCap transformCap filePaths = do
  stats <- foldM processOneFile initialStats filePaths
  return stats
  where
    processOneFile stats path = do
      result <- processSingleFile readCap writeCap transformCap path
      return $ updateStats stats result
```

## Path Transformation

Clod transforms file paths to make them more Claude-friendly:

```haskell
-- Transform a file path for Claude
transformFilePath :: FilePath -> OptimizedName
transformFilePath path =
  let parts = splitDirectories path
      transformedParts = map transformPathPart parts
      result = intercalate "/" transformedParts
  in OptimizedName result

-- Transform individual path components
transformPathPart :: String -> String
transformPathPart part
  | null part = ""
  | head part == '.' = "dot--" ++ tail part  -- Handle hidden files/dirs
  | otherwise = part
```

## Future Development

Potential areas for future enhancement:

1. Parallelized file processing for improved performance on large codebases
2. More sophisticated file transformations based on file type
3. Integration with cloud storage services
4. Remote file processing capabilities
5. GUI interface for easier configuration

## Development Guidelines

1. Follow capability-based security principles for all file operations
2. Use pure functions whenever possible
3. Handle errors with proper error types and messages
4. Ensure comprehensive test coverage for all components
5. Maintain backward compatibility with existing configurations