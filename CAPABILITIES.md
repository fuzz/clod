# Clod Capability System

This document describes Clod's capability-based security model that restricts file and Git repository access.

## Capability-Based Security Model

Clod implements a capability-based security model for file operations. A capability is essentially a token that grants permission to perform specific operations on a set of files or directories. 

The key principles are:

1. **Default Denial**: By default, no file access is permitted
2. **Explicit Capabilities**: Each operation requires an explicit capability token
3. **Restricted Paths**: Capabilities only allow operations within specific directory trees
4. **Capability Verification**: All operations verify capabilities before execution

This design was chosen to ensure safety when working with AI-generated code or when running Clod in sensitive environments.

## How It Works

### Capability Types

Clod defines three primary capability types:

1. **FileReadCap**: Grants permission to read files within specified directories
2. **FileWriteCap**: Grants permission to write files within specified directories
3. **GitCap**: Grants permission to perform Git operations on specified repositories

### Creating Capabilities

Capabilities are created with a list of directories where operations are permitted:

```haskell
-- Create a read capability for project and test directories
myReadCap = fileReadCap ["/home/user/project", "/home/user/tests"]

-- Create a write capability for output directory only
myWriteCap = fileWriteCap ["/home/user/output"]

-- Create a Git capability for the project repository
myGitCap = gitCap ["/home/user/project"]
```

### Using Capabilities

All file system operations that interact with the filesystem require appropriate capabilities:

```haskell
-- Read a file (requires FileReadCap)
content <- safeReadFile myReadCap "/home/user/project/src/Main.hs"

-- Write a file (requires FileWriteCap)
safeWriteFile myWriteCap "/home/user/output/result.txt" "Hello, world!"

-- Copy a file (requires both FileReadCap and FileWriteCap)
safeCopyFile myReadCap myWriteCap "/home/user/project/data.csv" "/home/user/output/data-copy.csv"

-- Get modified files from a Git repository (requires GitCap)
files <- safeGetModifiedFiles myGitCap "/home/user/project"
```

## Security Benefits

This capability-based approach provides several important security benefits:

1. **Path Traversal Prevention**: Files outside allowed directories cannot be accessed, even with path traversal attacks
2. **Explicit Permission Model**: The code clearly indicates which operations are permitted and where
3. **Principle of Least Privilege**: Components only get access to the specific directories they need
4. **Transparent Intentions**: Code that needs file access must explicitly request capabilities

## Implementation Details

The capability system is implemented using runtime checks:

1. When a file operation is requested, the path is canonicalized to resolve any `.`, `..`, or symlinks
2. The canonical path is checked against the directories allowed by the capability
3. If the path is within an allowed directory, the operation proceeds
4. If the path is outside allowed directories, an error is thrown

Example of the path verification logic:

```haskell
isPathAllowed :: [FilePath] -> FilePath -> IO Bool
isPathAllowed allowedDirs path = do
  -- Get canonical paths to resolve any `.`, `..`, or symlinks
  canonicalPath <- canonicalizePath path
  -- Check if the canonical path is within any of the allowed directories
  checks <- mapM (\dir -> do
                   canonicalDir <- canonicalizePath dir
                   -- A path is allowed if:
                   -- 1. It equals an allowed directory exactly, or
                   -- 2. It's a proper subdirectory (dir is a prefix and has a path separator)
                   let isAllowed = canonicalDir == canonicalPath || 
                                  (canonicalDir `isPrefixOf` canonicalPath && 
                                   length canonicalPath > length canonicalDir &&
                                   isPathSeparator (canonicalPath !! length canonicalDir))
                   return isAllowed) allowedDirs
  -- Return result
  return (or checks)
```

## Future Directions

For future versions of Clod, we're considering:

1. **More Granular Capabilities**: Adding more specialized capabilities (e.g., for specific operations)
2. **Type-Level Guarantees**: Exploring type-level verification of capabilities
3. **Better Error Messages**: Improving error messages for capability violations
4. **Capability Composition**: Making it easier to compose and transform capabilities