# Capability-Based Security

This document details the capability-based security patterns used in the project for secure file and resource access management.

## Core Concept

Capability-based security restricts operations based on explicit capability tokens rather than implicit permissions. A capability is an unforgeable token that grants specific permissions to perform operations.

## Implementation Principles

1. Define capabilities as tokens that grant specific permissions
2. Require these tokens as parameters for potentially unsafe operations
3. Check permissions at runtime before performing operations
4. Design test cases to verify capability restrictions work correctly

## File System Capabilities

### Core Capabilities

```haskell
-- Grant permission to read from specific directories
data FileReadCap = FileReadCap
  { allowedReadDirs :: [FilePath]   -- Directories from which reading is allowed
  , validateReadPath :: FilePath -> IO Bool  -- Function to validate paths
  }

-- Grant permission to write to specific directories
data FileWriteCap = FileWriteCap
  { allowedWriteDirs :: [FilePath]  -- Directories to which writing is allowed
  , validateWritePath :: FilePath -> IO Bool  -- Function to validate paths
  }

-- Grant permission to transform file contents
data FileTransformCap = FileTransformCap
  { transformFn :: BS.ByteString -> BS.ByteString  -- Transformation function
  }
```

### Path Validation

```haskell
-- Check if a path is within allowed directories
isPathAllowed :: [FilePath] -> FilePath -> IO Bool
isPathAllowed allowedDirs path = do
  -- Get canonical path to resolve symlinks and normalize
  canonicalPath <- canonicalizePath path
  let normalizedPath = normalise canonicalPath
  
  -- Check if path is within any of the allowed directories
  results <- forM allowedDirs $ \dir -> do
    canonicalDir <- canonicalizePath dir
    let normalizedDir = normalise canonicalDir
    return $ normalizedDir `isPrefixOf` normalizedPath
  
  -- Path is allowed if it's within at least one allowed directory
  return $ or results
```

### Secure Operations

```haskell
-- Secure file reading with capability check
safeReadFile :: FileReadCap -> FilePath -> IO (Either Error BS.ByteString)
safeReadFile cap path = do
  -- Verify file is within allowed directories
  allowed <- validateReadPath cap path
  if not allowed
    then return $ Left $ SecurityError $ 
      "Access denied: Cannot read file outside allowed directories: " ++ path
    else do
      -- Perform the operation once validated
      try $ BS.readFile path

-- Secure file writing with capability check
safeWriteFile :: FileWriteCap -> FilePath -> BS.ByteString -> IO (Either Error ())
safeWriteFile cap path content = do
  -- Verify file is within allowed directories
  allowed <- validateWritePath cap path
  if not allowed
    then return $ Left $ SecurityError $ 
      "Access denied: Cannot write file outside allowed directories: " ++ path
    else do
      -- Create directory if it doesn't exist
      createDirectoryIfMissing True (takeDirectory path)
      -- Perform the operation once validated
      try $ BS.writeFile path content
```

## Applying Capabilities in the Application

### Capability Construction

```haskell
-- Create a read capability with specific allowed directories
makeReadCap :: [FilePath] -> IO FileReadCap
makeReadCap dirs = do
  -- Canonicalize all directories for consistent path comparison
  canonicalDirs <- mapM canonicalizePath dirs
  return $ FileReadCap
    { allowedReadDirs = canonicalDirs
    , validateReadPath = isPathAllowed canonicalDirs
    }

-- Create a write capability with specific allowed directories
makeWriteCap :: [FilePath] -> IO FileWriteCap
makeWriteCap dirs = do
  -- Canonicalize all directories for consistent path comparison
  canonicalDirs <- mapM canonicalizePath dirs
  return $ FileWriteCap
    { allowedWriteDirs = canonicalDirs
    , validateWritePath = isPathAllowed canonicalDirs
    }
```

### Usage in Application Logic

```haskell
runApp :: Config -> IO Result
runApp config = do
  -- Create capabilities with appropriate permissions
  readCap <- makeReadCap [config.sourceDir]
  writeCap <- makeWriteCap [config.outputDir]
  
  -- Use capabilities in operations
  result <- processFiles readCap writeCap config.files
  return result

-- Processing logic that requires capabilities
processFiles :: FileReadCap -> FileWriteCap -> [FilePath] -> IO Result
processFiles readCap writeCap files = do
  forM_ files $ \file -> do
    -- Use capability-protected operations
    contentResult <- safeReadFile readCap file
    case contentResult of
      Left err -> logError $ "Cannot read file: " ++ show err
      Right content -> do
        -- Process content and write output
        let processedContent = processContent content
        writeResult <- safeWriteFile writeCap (outputPath file) processedContent
        case writeResult of
          Left err -> logError $ "Cannot write file: " ++ show err
          Right _ -> return ()
  return Success
```

## Detailed Security Error Messages

```haskell
-- Provide specific reasons for access denials
throw $ SecurityError $ "Access denied: Source path violates restrictions: " ++ 
  canonicalPath ++ " is not within allowed directories: " ++ 
  show (allowedReadDirs readCap)
```

## Testing Capability Restrictions

```haskell
it "respects read capability restrictions" $ do
  withSystemTempDirectory "test-dir" $ \tmpDir -> do
    -- Create test directories
    let sourceDir = tmpDir </> "source"
        outputDir = tmpDir </> "output"
        illegalDir = tmpDir </> "illegal"
    
    -- Create directories and test files
    createDirectoryIfMissing True sourceDir
    createDirectoryIfMissing True outputDir
    createDirectoryIfMissing True illegalDir
    
    -- Create test files
    writeFile (sourceDir </> "test.txt") "test content"
    writeFile (illegalDir </> "illegal.txt") "illegal content"
    
    -- Create capabilities with restricted access
    readCap <- makeReadCap [sourceDir]
    writeCap <- makeWriteCap [outputDir]
    
    -- Test legal access (should succeed)
    readResult1 <- safeReadFile readCap (sourceDir </> "test.txt")
    isRight readResult1 `shouldBe` True
    
    -- Test illegal access (should fail with security error)
    readResult2 <- safeReadFile readCap (illegalDir </> "illegal.txt")
    isLeft readResult2 `shouldBe` True
    case readResult2 of
      Left (SecurityError _) -> return () -- Expected error type
      Left other -> expectationFailure $ "Expected SecurityError but got: " ++ show other
      Right _ -> expectationFailure "Expected access to be denied"
```

## Benefits of Capability-Based Security

1. **Explicit Permission Model**: Makes security boundaries explicit in the code
2. **Least Privilege**: Operations only have access to what they explicitly need
3. **Composable Security**: Capabilities can be combined and passed selectively
4. **Testable**: Security restrictions can be verified through automated tests
5. **Audit Trail**: Clear visibility into what operations can access what resources

## Implementation in Different Paradigms

### Monad Transformers Approach

```haskell
-- Using ReaderT for capabilities
type AppM a = ReaderT AppCapabilities (ExceptT Error IO) a

data AppCapabilities = AppCapabilities
  { readCap :: FileReadCap
  , writeCap :: FileWriteCap
  }

-- Use capabilities from the reader environment
readFileSecurely :: FilePath -> AppM BS.ByteString
readFileSecurely path = do
  cap <- asks readCap
  result <- liftIO $ safeReadFile cap path
  case result of
    Left err -> throwError err
    Right content -> return content
```

### Effect Systems Approach

```haskell
-- Define file system effect with capability constraints
data FileSystem m a where
  ReadFile :: FileReadCap -> FilePath -> FileSystem m BS.ByteString
  WriteFile :: FileWriteCap -> FilePath -> BS.ByteString -> FileSystem m ()

-- Generate effect functions
makeSem ''FileSystem

-- Interpreter that checks capabilities
runFileSystem :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystem = interpret $ \case
  ReadFile cap path -> do
    result <- embed $ safeReadFile cap path
    case result of
      Left err -> throw err
      Right content -> return content
  WriteFile cap path content -> do
    result <- embed $ safeWriteFile cap path content
    case result of
      Left err -> throw err
      Right () -> return ()
```