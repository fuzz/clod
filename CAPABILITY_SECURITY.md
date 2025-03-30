# Capability-Based Security

This document details the capability-based security patterns used in Clod for secure file and resource access management.

## Core Concept

Capability-based security restricts operations based on explicit capability tokens rather than implicit permissions. A capability is an unforgeable token that grants specific permissions to perform operations.

The key principles are:
1. **Default Denial**: By default, no file access is permitted
2. **Explicit Capabilities**: Each operation requires an explicit capability token
3. **Restricted Paths**: Capabilities only allow operations within specific directory trees
4. **Capability Verification**: All operations verify capabilities before execution

## Implementation Approaches

Clod implements two capability-based security approaches:

### 1. Standard Runtime Capability System

The standard system is implemented in `Clod.Types` and used throughout the application:

```haskell
-- Grant permission to read from specific directories
data FileReadCap = FileReadCap 
  { allowedReadDirs :: [FilePath] -- Directories where reading is permitted
  } deriving (Show, Eq)

-- Grant permission to write to specific directories
data FileWriteCap = FileWriteCap 
  { allowedWriteDirs :: [FilePath] -- Directories where writing is permitted
  } deriving (Show, Eq)

-- Create capabilities
fileReadCap :: [FilePath] -> FileReadCap
fileReadCap dirs = FileReadCap { allowedReadDirs = dirs }

fileWriteCap :: [FilePath] -> FileWriteCap
fileWriteCap dirs = FileWriteCap { allowedWriteDirs = dirs }
```

### 2. Advanced Type-Level Capability System

The advanced system in `Clod.AdvancedCapability` uses type-level programming to enforce permissions at compile-time:

```haskell
-- Permission types for capabilities
data Permission = Read | Write | Execute | All

-- A path with type-level permission information
data TypedPath (p :: Permission) where
  TypedPath :: FilePath -> TypedPath p

-- Capability token that grants permissions
data Capability (p :: Permission) = Capability 
  { allowedDirs :: [FilePath]  -- Directories this capability grants access to
  }

-- Create a capability token for the given permission and directories
createCapability :: forall p. [FilePath] -> Capability p
createCapability dirs = Capability { allowedDirs = dirs }
```

## Path Validation

Both implementations use similar path validation logic to prevent path traversal attacks:

```haskell
-- Check if a path is within allowed directories
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
  where
    isPathSeparator c = c == '/' || c == '\\'
```

## Secure Operations

All file system operations require appropriate capabilities:

```haskell
-- Safe file reading that checks capabilities
safeReadFile :: FileReadCap -> FilePath -> ClodM BS.ByteString
safeReadFile cap path = do
  allowed <- liftIO $ isPathAllowed (allowedReadDirs cap) path
  if allowed
    then liftIO $ BS.readFile path
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ 
        "Access denied: Cannot read file outside allowed directories: " ++ canonicalPath

-- Safe file writing that checks capabilities
safeWriteFile :: FileWriteCap -> FilePath -> BS.ByteString -> ClodM ()
safeWriteFile cap path content = do
  allowed <- liftIO $ isPathAllowed (allowedWriteDirs cap) path
  if allowed
    then liftIO $ BS.writeFile path content
    else do
      canonicalPath <- liftIO $ canonicalizePath path
      throwError $ CapabilityError $ 
        "Access denied: Cannot write file outside allowed directories: " ++ canonicalPath

-- Safe file copying that checks capabilities for both read and write
safeCopyFile :: FileReadCap -> FileWriteCap -> FilePath -> FilePath -> ClodM ()
safeCopyFile readCap writeCap src dest = do
  srcAllowed <- liftIO $ isPathAllowed (allowedReadDirs readCap) src
  destAllowed <- liftIO $ isPathAllowed (allowedWriteDirs writeCap) dest
  if srcAllowed && destAllowed
    then liftIO $ copyFile src dest
    else throwError $ CapabilityError $ 
      "Access denied: Path restrictions violated"
```

## Type-Level Operations (Advanced System)

The advanced system provides type-safe file operations:

```haskell
-- Read a file with the given capability
readFile :: forall p m. (MonadIO m, PermissionFor 'Read p) 
         => Capability p -> TypedPath p -> m BS.ByteString
readFile _ (TypedPath path) = liftIO $ BS.readFile path

-- Write to a file with the given capability
writeFile :: forall p m. (MonadIO m, PermissionFor 'Write p) 
          => Capability p -> TypedPath p -> BS.ByteString -> m ()
writeFile _ (TypedPath path) content = liftIO $ BS.writeFile path content

-- Check if a path is allowed by this capability and create a typed path if it is
withPath :: forall p m a. (MonadIO m) 
         => Capability p -> FilePath -> (Maybe (TypedPath p) -> m a) -> m a
withPath cap path f = do
  allowed <- liftIO $ isPathAllowed (allowedDirs cap) path
  f $ if allowed then Just (TypedPath path) else Nothing
```

## Using Capabilities in the Application

Capabilities are typically created at the application entry point and passed down to functions that need them:

```haskell
runApp :: Config -> IO ()
runApp config = do
  -- Create capabilities with appropriate permissions
  let readCap = fileReadCap [config.sourceDir]
  let writeCap = fileWriteCap [config.outputDir]
  
  -- Use capabilities in operations
  result <- runClodM config $ do
    processFiles readCap writeCap config.files
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Processing complete"
```

## Security Benefits

This capability-based approach provides several important security benefits:

1. **Path Traversal Prevention**: Files outside allowed directories cannot be accessed, even with path traversal attacks
2. **Explicit Permission Model**: The code clearly indicates which operations are permitted and where
3. **Principle of Least Privilege**: Components only get access to the specific directories they need
4. **Transparent Intentions**: Code that needs file access must explicitly request capabilities
5. **Compile-Time Checks**: The advanced system catches permission errors at compile time with type-level constraints
6. **Composable Security**: Capabilities can be restricted and combined
7. **Testable**: Security restrictions can be verified through automated tests

## Future Directions

For future versions of Clod, we're considering:

1. **More Granular Capabilities**: Adding more specialized capabilities (e.g., for specific operations)
2. **Enhanced Type-Level Guarantees**: Extending the type-level verification of capabilities
3. **Better Error Messages**: Improving error messages for capability violations
4. **Capability Composition**: Making it easier to compose and transform capabilities
5. **Effect Integration**: Deeper integration with algebraic effects systems