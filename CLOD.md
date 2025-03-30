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
  - `Clod.FileSystem.Detection`: File type detection
  - `Clod.FileSystem.Operations`: File operations
  - `Clod.FileSystem.Processing`: File processing
  - `Clod.FileSystem.Transformations`: Text transformations
- `Clod.Output`: Output formatting and display
- `Clod.IgnorePatterns`: Pattern matching for ignored files
- `Clod.Capability`: Capability-based security
- `Clod.Config`: Configuration management
- `Clod.Git`: Git-related functionality (legacy)

### Command-Line Interface

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

### Database File Format

Clod stores file tracking information in a Dhall-formatted database file:

```
-- Clod Database
-- Format: Dhall
-- Generated: 2023-10-15 14:30:45 UTC

{ serializedFiles = [("/path/to/file.txt", { entryPath = "/path/to/file.txt", entryChecksum = "abc123...", entryLastModified = "2023-10-15 14:25:30 UTC", entryOptimizedName = "file.txt" })],
  serializedChecksums = [("abc123...", "/path/to/file.txt")],
  serializedLastStagingDir = "/path/to/staging",
  serializedLastRunTime = "2023-10-15 14:30:45 UTC"
}
```

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

Clod employs multiple strategies for reliable binary file detection:

1. Extension-based detection using configured lists of text and binary extensions
2. Magic byte detection using a database of binary file signatures
3. Content analysis:
   - Null byte detection
   - Control character ratio analysis
   - UTF-8 validity checking

The strategy uses a combination of these approaches for maximum reliability.

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
├── manifest.json
└── ...
```

The `--last` flag allows reusing the previous staging directory instead of creating a new one.

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