# Haskell Patterns and Best Practices

This document contains common Haskell patterns and best practices extracted from the project. It serves as a reference for efficient and idiomatic Haskell development.

## Functional Programming Patterns

- **Pure Functions**: Prefer pure functions over shell commands and impure operations
- **Resource Management**: Always handle resources properly with patterns like `bracket`
- **Error Handling**: Wrap impure code with `try`/`catch` and convert exceptions to domain-specific errors
- **Type Safety**: Use `newtype` wrappers to improve type safety
- **Higher-Order Functions**: Leverage higher-order functions for operations on lists and collections

## Module Organization

- Follow hierarchical module structure (e.g., `App.Module.Submodule`)
- Create facade modules that re-export functionality from specialized modules
- Maintain backward compatibility by keeping public APIs stable
- Integrate new functionality through appropriate facade modules

## Advanced Type System Features

### Type Aliases and Constraints

```haskell
-- Use ConstraintKinds for constraint type aliases
{-# LANGUAGE ConstraintKinds #-}
type ErrorHandler r = Member (Error e) r
```

### Phantom Types

```haskell
-- Use coerce for type conversions with phantom types
import Data.Coerce (coerce)
newtype TypedPath a = TypedPath FilePath

-- Converting between phantom types
srcPath :: TypedPath Source
destPath :: TypedPath Dest
convertPath :: TypedPath Source -> TypedPath Dest
convertPath = coerce
```

### Language Extensions

- Pattern synonyms: `PatternSynonyms`, `ViewPatterns`
- Type families: `TypeFamilies`, `UndecidableInstances`
- GADTs: `GADTs`, `DataKinds`
- Deriving mechanisms: `DeriveGeneric`, `DerivingVia`
- Complex kind signatures: `KindSignatures`

### Kleisli Composition

```haskell
import Control.Arrow ((>>>), (<<<), Kleisli(..), runKleisli)

-- Create Kleisli arrows for monadic functions
step1K = Kleisli $ \input -> step1 input
step2K = Kleisli $ \input -> step2 input

-- Compose them
pipeline = step1K >>> step2K >>> step3K

-- Run the pipeline
result <- runKleisli pipeline initialInput
```

## Error Handling

```haskell
-- Use monad transformer stack for error handling
type AppM a = ReaderT Config (ExceptT Error IO) a

-- Define clear error types
data Error
  = FileSystemError FilePath IOError
  | ConfigError String
  | SecurityError String
  deriving (Show, Eq)

-- Convert exceptions to domain-specific errors
safeFileOperation :: FilePath -> AppM ByteString
safeFileOperation path = do
  result <- liftIO $ try $ readFile path
  case result of
    Left e -> throwError $ FileSystemError path e
    Right content -> return content
```

## Resource Management

```haskell
-- Use bracket for resource acquisition and release
withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile action = bracket
  (openTempFile "/tmp" "temp.txt")  -- acquire
  (\(path, handle) -> do            -- release
     hClose handle
     removeFile path)
  (\(path, handle) -> action path handle)  -- use
```

## Testing Techniques

### Marking Tests as Pending

```haskell
it "test that should be changed" $
  pendingWith "This test is being updated after core refactoring"
```

### Test Environment Setup

```haskell
-- Helper for setting up a test environment
withTestEnvironment :: (FilePath -> IO ()) -> IO ()
withTestEnvironment runTest = 
  withSystemTempDirectory "test-dir" $ \tmpDir -> do
    -- Create test files
    createDirectoryIfMissing True (tmpDir </> "src")
    writeFile (tmpDir </> "src" </> "test.file") "test content"
    -- Run the test with the prepared environment
    runTest tmpDir

-- Use it in tests
it "tests something" $
  withTestEnvironment $ \tmpDir -> do
    -- Your test code here
```

### Resilient File Path Testing

```haskell
-- Instead of checking for an exact filename:
doesFileExist (tmpDir </> "staging" </> "file.txt") `shouldBe` True

-- Check for files containing a distinctive part of the filename:
allFiles <- getDirectoryContents (tmpDir </> "staging")
let matchingFiles = filter (\f -> "file" `isInfixOf` f && f `notElem` [".", ".."]) allFiles
(not (null matchingFiles)) `shouldBe` True
```

### Debugging Output in Tests

```haskell
it "respects ignore patterns" $ do
  -- Test setup
  withSystemTempDirectory "test-dir" $ \tmpDir -> do
    -- Run test
    result <- runApp tmpDir
    
    -- Debug output to understand what's happening
    allFiles <- getDirectoryContents (tmpDir </> "output")
    putStrLn $ "Files in output: " ++ show allFiles
    
    -- Assertions
    result `shouldBe` Success
```

## Debugging Techniques

### Isolated Incremental Testing

```haskell
-- Original complex function
complexProcess :: Config -> [Input] -> IO [Output]

-- Broken into smaller, testable parts
step1 :: Config -> [Input] -> IO [IntermediateA]
step2 :: [IntermediateA] -> IO [IntermediateB]
step3 :: [IntermediateB] -> IO [Output]

-- Compose them back together
complexProcess config inputs = do
  resultA <- step1 config inputs
  resultB <- step2 resultA
  step3 resultB
```

### Strategic Tracing

```haskell
import Debug.Trace (trace)

-- Add tracing to a key function while debugging
processFile :: FilePath -> IO Result
processFile path = do
  let debugPath = trace ("Processing file: " ++ path) path
  exists <- doesFileExist debugPath
  if exists
    then trace "File exists, continuing..." $ do
      content <- readFile debugPath
      trace ("Content length: " ++ show (length content)) (return ())
      -- Process content
    else trace "File doesn't exist!" $ return NotFound
```

## Working with ByteStrings

```haskell
-- Converting between ByteString and String
stringToBS :: String -> BS.ByteString
stringToBS = BS.pack . map (fromIntegral . fromEnum)

bsToString :: BS.ByteString -> String
bsToString = map (toEnum . fromIntegral) . BS.unpack
```

## Cabal Configuration

- Use `other-modules: Paths_<package>` to expose auto-generated Paths module
- List all public modules in `exposed-modules` field
- Include resource files in `extra-source-files`
- Specify package dependencies with version ranges
- Document system dependencies in README

## Version Number Management

```haskell
-- Access version from cabal file
import qualified Paths_<package> as Meta
import Data.Version (showVersion)

-- Display version
version :: String
version = showVersion Meta.version
```

## Best Practices

- Favor pure Haskell implementations over shell commands
- Document system dependencies explicitly
- Load resource files from standardized locations, not hardcoded paths
- Derive version information from the cabal file, not hardcoded
- Use type applications for parametric types
- Normalize paths for cross-platform compatibility
- Add explicit type annotations for complex expressions