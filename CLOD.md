# Clod Project Documentation

This document contains specific information about the Clod project implementation, design decisions, and architectural details.

## Project Overview

Clod (Claude Optimization Driver) is a tool designed to prepare text files for optimal processing by Claude AI models. It helps:

- Transform text files to be more Claude-friendly
- Generate manifest files for mapping between original and transformed paths
- Create staging directories with optimized files
- Skip binary files automatically
- Respect ignore patterns (.gitignore, .clodignore)
- Detect file changes efficiently

## Key Components

### Checksum-Based File Tracking

Clod uses a checksum-based approach for tracking file changes:

- Checksums detect file content changes regardless of modification time
- Works in any directory structure without requiring a Git repository
- Detects file renames by matching identical content with different paths
- Provides a consistent experience across different environments

Checksums are implemented using SHA-256:

```haskell
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16

-- Calculate SHA-256 checksum of a ByteString
calculateChecksum :: BS.ByteString -> Checksum
calculateChecksum content =
  let hash = SHA256.hash content
      hexHash = Base16.encode hash
  in Checksum (show hexHash)
```

The `checksumFile` function safely calculates checksums for text files:

```haskell
-- | Calculate the checksum of a file
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
        then throwError $ ChecksumError "Cannot checksum binary file"
        else do
          -- Read file content and calculate checksum
          content <- safeReadFile readCap path
          return $ calculateChecksum content
```

### Database Structure

The database structure enables efficient queries:

- `dbFiles` maps file paths to their entries for quick lookup by path
- `dbChecksums` maps checksums to paths for detecting renamed files
- `dbLastStagingDir` stores the previous staging directory for the `--last` flag
- Entries contain the full file metadata including content hash, modification time, and optimized name

### Using Dhall for Serialization

Clod uses Dhall for type-safe configuration and database serialization:

```haskell
-- | Serialization-friendly version of ClodDatabase
data SerializableClodDatabase = SerializableClodDatabase
  { serializedFiles          :: ![(FilePath, FileEntry)]
  , serializedChecksums      :: ![(String, FilePath)]
  , serializedLastStagingDir :: !(Maybe FilePath)
  , serializedLastRunTime    :: !UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)
    
-- Convert to/from serializable form
toSerializable :: ClodDatabase -> SerializableClodDatabase
toSerializable db = SerializableClodDatabase
  { serializedFiles = Map.toList (dbFiles db)
  , serializedChecksums = Map.toList (dbChecksums db)
  , serializedLastStagingDir = dbLastStagingDir db
  , serializedLastRunTime = dbLastRunTime db
  }

fromSerializable :: SerializableClodDatabase -> ClodDatabase
fromSerializable sdb = ClodDatabase
  { dbFiles = Map.fromList (serializedFiles sdb)
  , dbChecksums = Map.fromList (serializedChecksums sdb)
  , dbLastStagingDir = serializedLastStagingDir sdb
  , dbLastRunTime = serializedLastRunTime sdb
  }
```

## Project Architecture

### Module Structure

- `Clod.Types`: Core data types and type classes
- `Clod.Core`: Main application logic
- `Clod.FileSystem`: File system operations
  - `Clod.FileSystem.Checksums`: Checksum-based file tracking
  - `Clod.FileSystem.Detection`: File type detection (using magic library)
  - `Clod.FileSystem.Operations`: File operations
  - `Clod.FileSystem.Processing`: File processing
  - `Clod.FileSystem.Transformations`: Text transformations
- `Clod.Output`: Output formatting and display
- `Clod.IgnorePatterns`: Pattern matching for ignored files
- `Clod.Capability`: Capability-based security
- `Clod.Config`: Configuration management

Note: Prior versions of Clod included Git functionality (in modules like `Clod.Git`), but these have been removed in favor of the checksums-based approach.

### Command-Line Interface

Clod provides several command-line options:

- `--verbose`: Enable verbose output
- `--staging`: Specify a custom staging directory
- `--flush`: Flush stale entries from the database
  - Removes file entries from the database that no longer exist on disk
  - Useful for cleaning up the database after files have been deleted
- `--last`: Use the previous staging directory
  - Returns the path of the most recent staging directory stored in the database
  - Exits immediately after outputting the path
  - Useful for scripts that need to access the previous run's output
- `--all-files`: Process all files, not just new or modified ones
- `--modified`: Process only modified files
- `--quiet`: Suppress non-essential output
- `--version`: Show version information
- `--help`: Show help text

### Database File Format

Clod stores file tracking information in a Dhall-formatted database file:

```dhall
-- Clod Database
-- Format: Dhall
-- Generated: 2023-10-15 14:30:45 UTC

{ serializedFiles = [
    { _1 = "/path/to/file.txt"
    , _2 = { entryPath = "/path/to/file.txt"
           , entryChecksum = { unChecksum = "abc123..." }
           , entryLastModified = { date = "2023-10-15"
                                 , time = "14:25:30.123456"
                                 , timeZone = "UTC" 
                                 }
           , entryOptimizedName = { unOptimizedName = "file.txt" }
           }
    }
  ]
, serializedChecksums = [
    { _1 = "abc123..."
    , _2 = "/path/to/file.txt"
    }
  ]
, serializedLastStagingDir = Some "/path/to/staging"
, serializedLastRunTime = { date = "2023-10-15"
                          , time = "14:30:45.789012"
                          , timeZone = "UTC"
                          }
}
```

Note: The Dhall format is strongly typed, which means empty lists need explicit type annotations (e.g., `[] : List { _1 : Text, _2 : Text }`) and composite types like `UTCTime` are represented as records with fields for date, time, and timezone components.

## File Change Detection

Clod identifies the following file statuses:

1. `New`: Files not previously in the database
2. `Modified`: Files with a different checksum than the database entry
3. `Unchanged`: Files with the same checksum as the database entry
4. `Deleted`: Files that were in the database but don't exist anymore
5. `Renamed`: Files with an identical checksum but different path

The detection algorithm:

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

Clod employs the `magic` library for reliable binary file detection. This is an improvement over heuristic approaches as it:

1. Uses the same detection mechanism as the Unix `file` command
2. Identifies files by examining their content signatures
3. Provides accurate MIME type classification
4. Works consistently across different platforms

The implementation in `Clod.FileSystem.Detection` provides these key functions:

```haskell
-- | Check if a file is a text file using libmagic
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

-- | Helper to determine if a file description indicates text content
isTextDescription :: String -> Bool
isTextDescription desc =
  let lowerDesc = map toLower desc
      textPatterns = 
        [ "text"
        , "ascii"
        , "unicode"
        , "utf"
        , "json"
        , "xml"
        , "yaml"
        , "source"
        , "script"
        , "html"
        , "css"
        -- plus additional patterns for common text formats
        ]
  in any (`isInfixOf` lowerDesc) textPatterns
```

This approach is more reliable than previous methods that relied on heuristics like control character analysis or null byte detection, and also more comprehensive than simply checking MIME type prefixes, as it recognizes many text formats (JSON, YAML, etc.) that don't use the "text/" MIME type prefix.

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
├── _path_manifest.json
└── ...
```

The `--last` flag functionality allows using the previous staging directory:

1. When running Clod with the `--last` flag, it:
   - Retrieves the previous staging directory path from the database
   - Prints the path to stdout
   - Exits with a special error code
   - No files are processed when using this flag

2. The implementation in `Clod.Core` handles this flag:

```haskell
-- | Part of the mainLogic function
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

3. Each time Clod runs, it updates the database with the current staging directory:

```haskell
-- Create updated database with the current staging directory
databaseWithStaging = finalDatabase { 
    dbLastStagingDir = Just stagingDir,
    dbLastRunTime = dbLastRunTime finalDatabase 
  }

-- Save the updated database
saveDatabase databaseFile databaseWithStaging
```

This approach ensures the database always contains the most recent staging directory for use with the `--last` flag.

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