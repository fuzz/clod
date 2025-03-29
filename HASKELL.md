# Haskell Development Notes

This document contains learning points about working with Haskell from Claude Code sessions.

## Module Organization and Package Structure

- Haskell projects typically follow a hierarchical module structure (e.g., `Clod.Git.LibGit`).
- The standard pattern is to create facade modules that re-export functionality from specialized modules.
- When refactoring, maintain backward compatibility by keeping public APIs stable.
- When adding new functionality (like the `LibGit` module), integrate it through the facade module.

## Cabal Configuration

- Use `other-modules: Paths_clod` to expose the auto-generated Paths module.
- The `exposed-modules` field should list all modules intended to be public.
- Use `extra-source-files` for including resource files, templates, etc.
- Package dependencies should be specified with version ranges.
- System dependencies (like libgit2) should be documented in the README.

## Functional Programming Patterns

- Prefer pure functions over shell commands and impure operations.
- Always handle resources properly with patterns like `bracket`.
- Wrap impure code with `try`/`catch` and convert exceptions to domain-specific errors.
- Use `newtype` wrappers to improve type safety.
- Leverage higher-order functions for operations on lists and collections.

## Version Number Management

- Use the auto-generated `Paths_<package>` module to access the version from the cabal file.
- Import it as qualified: `import qualified Paths_clod as Meta`.
- Access the version with `showVersion Meta.version`.
- This ensures the displayed version stays in sync with the package version.

## FFI and Foreign Libraries

- When integrating with C libraries like libgit2, use bracket patterns for resource management.
- Be explicit about types when converting between C and Haskell types to avoid ambiguity.
- Wrap low-level FFI calls in higher-level, type-safe APIs.
- Document system dependencies and installation requirements for FFI libraries.
- Use cabal.project.local for specifying paths to system libraries.

## Error Handling

- Use a monad transformer stack (like `ReaderT` + `ExceptT`) for error handling.
- Define clear error types in domain-specific modules.
- Convert exceptions to domain-specific errors for better error messages.
- Use `try` and `catch` for handling exceptions from IO operations.

## Resource Management

- Use `bracket` for proper resource acquisition and release.
- The bracket pattern is: `bracket acquire release action`.
- Temporary directories should be cleaned up when no longer needed.
- File handles and other resources should be properly closed.

## Testing Techniques

- **Marking Tests as Pending**: When a test is no longer relevant due to architectural changes, mark it as pending:
  ```haskell
  it "some test that should be changed" $
    pendingWith "This test is being updated after core refactoring"
  ```
  
- **Test Counting with Pending Tests**: When using Hspec, pending tests are counted separately:
  - When the test report shows `123 examples, 0 failures, 1 pending`, it means:
    - 123 tests were executed and passed
    - 1 additional test was marked as pending (not executed)
    - The total is therefore 124 tests
  - Fixing a previously pending test will show `123 examples, 0 failures` (all tests passing)

- **Complex Test Fixtures**: When a test fixture is complex, break it into helper functions:
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

- **Generic Filesystem Test Helpers**: Create reusable functions for file-related tests:
  ```haskell
  -- Helper for finding files in a test directory
  findAllFiles' :: FilePath -> IO [FilePath]
  findAllFiles' root = do
    allDirs <- listDirectories root
    paths <- forM allDirs $ \dir -> do
      entries <- getDirectoryContents dir
      let validEntries = filter (`notElem` [".", ".."]) entries
      return $ map (makeRelative root . (dir </>)) validEntries
    return $ concat paths
    
  -- Helper for creating test files
  createTestFiles :: FilePath -> [(FilePath, String)] -> IO ()
  createTestFiles base files = forM_ files $ \(path, content) -> do
    let fullPath = base </> path
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath content
  ```

- **Resilient File Path Testing**: When testing file operations, make tests resilient to different file naming implementations:
  ```haskell
  -- Instead of checking for an exact filename:
  doesFileExist (tmpDir </> "staging" </> "file.txt") `shouldBe` True
  
  -- Check for files containing a distinctive part of the filename:
  allFiles <- getDirectoryContents (tmpDir </> "staging")
  let matchingFiles = filter (\f -> "file" `isInfixOf` f && f `notElem` [".", ".."]) allFiles
  (not (null matchingFiles)) `shouldBe` True
  ```

- **Debugging Output in Tests**: Add temporary debug output for diagnosing test failures:
  ```haskell
  it "respects ignore patterns" $ do
    -- Test setup
    withSystemTempDirectory "test-dir" $ \tmpDir -> do
      -- Run test
      result <- runApp tmpDir
      
      -- Debug output to understand what's happening
      allFiles <- getDirectoryContents (tmpDir </> "output")
      putStrLn $ "Files in output: " ++ show allFiles
      
      -- Check manifest contents if it exists
      manifestExists <- doesFileExist (tmpDir </> "output" </> "manifest.json")
      when manifestExists $ do
        content <- readFile (tmpDir </> "output" </> "manifest.json")
        putStrLn $ "Manifest content: " ++ take 500 content
      
      -- Assertions
      result `shouldBe` Success
  ```

- **Exception-Catching in Tests**: Use explicit exception catching to enhance debugging:
  ```haskell
  import Control.Exception (catch, throwIO, SomeException)
  
  -- Add try/catch around assertions that might fail with details
  (manifestContent `shouldContain` "expected_text") 
    `catch` (\(e :: SomeException) -> do
              putStrLn $ "Error in shouldContain: " ++ show e
              putStrLn $ "Manifest content: " ++ manifestContent
              throwIO e)
  ```

## Testing

- Tests should not rely on specific project state or external resources.
- Use fixtures for testing modules that interact with the filesystem.
- Mock external dependencies for reproducible tests.

## Configuration Management

- Avoid hardcoded configuration; load it from files or environment variables.
- Follow the principle of least surprise by using standard locations.
- Provide clear fallbacks when configuration isn't found.
- Document all configuration options and their defaults.

## Integration with External Commands

- When calling external commands is necessary (like 'open' for file browsers):
  - Document the dependency clearly.
  - Handle potential errors (command not found, permission issues).
  - Be explicit about platform assumptions.

## Working with Bits and Binary Data

- When working with bit flags (like in Git status), be explicit about types.
- Use type annotations (`:: Int`, `:: CInt`) to avoid type defaulting warnings.
- Import Data.Bits for bit manipulation operations (`.&.`, `.|.`).

## Preferences and Philosophy

- Favor pure Haskell implementations over shell commands.
- System dependencies should be explicitly documented, not silently assumed.
- When a system dependency is required, make it obvious to users.
- Resource files should be loaded from standardized locations, not hardcoded.
- Version information should be derived from the cabal file, not hardcoded.

## Capability-Based Security

- Implement capability-based security for operations with potential security implications:
  - Define a capability as a token that grants specific permissions (e.g., read access to specific directories).
  - Require these tokens as parameters for any potentially unsafe operations.
  - Check permissions at runtime before performing the operation.
- Design test cases specifically to verify that capability restrictions work correctly.
- For file system operations, use path prefix checking to restrict access to allowed directories.
- Use the containing directory structure for temporary outputs to maintain clean repository state.
- Add output directories to .gitignore to avoid committing generated artifacts.

## Effect Systems vs. Monad Transformers

- Algebraic effect systems (like Polysemy) provide type-level tracking of effects but can introduce complexity:
  - Complex type errors that are difficult to debug
  - Potential overlapping instances issues
  - Dependency on Template Haskell for code generation
  - Can be difficult for newcomers to understand
  
- Traditional monad transformer stacks offer a simpler alternative:
  - More straightforward error messages
  - Better tooling and IDE support
  - Familiar to most Haskell developers
  - Easier to integrate with third-party libraries
  
- When migrating from effects to monad transformers:
  1. Create a monad stack type alias (e.g., `type ClodM a = ReaderT Config (ExceptT Error IO) a`)
  2. Replace effect handlers with direct monad operations:
     - `Member (Reader Config) r => ask` becomes just `ask`
     - `Member (Error MyError) r => throw` becomes `throwError`
     - `embed $ liftIO` becomes simply `liftIO`
  3. Update all function signatures to use the concrete monad type
  4. Add helper functions for running the monad stack
  5. Add re-exports of common operations to maintain a clean API
  
- Capability-based security works equally well with both approaches:
  - With effects: use capability tokens in functions with effect constraints
  - With monad transformers: use capability tokens in functions in your monad stack
  
- For maintainable code, you can start with simpler monad transformers and later migrate to effects if needed
  - This avoids premature complexity while preserving the core security model
  - Simpler code is easier for both humans and AI to maintain

## Debugging Techniques

- **Isolated Incremental Testing**: When debugging complex functional code, isolate and test components incrementally:
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

- **Debug Output in Tests**: Add strategic debug output to tests without modifying production code:
  ```haskell
  it "should process files correctly" $ do
    -- Setup test
    withSystemTempDirectory "test-dir" $ \tmpDir -> do
      -- Create test files
      
      -- Add debug output only in tests
      result <- runTestM $ do
        files <- findFiles "src"
        liftIO $ putStrLn $ "Files found: " ++ show files
        processFiles files
        
      -- More debugging after the operation
      liftIO $ do
        putStrLn $ "Result: " ++ show result
        content <- readFile (tmpDir </> "output")
        putStrLn $ "Output content: " ++ content
        
      -- Assertions (use shouldContain for partial matching)
      result `shouldBe` Success
  ```

- **Strategic Tracing**: Add temporary trace output for debugging complex issues:
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

- **Dealing with Ambiguous Types**: Use explicit type annotations to resolve ambiguities:
  ```haskell
  -- Ambiguous type in complex expression
  result <- runAction config (processor inputs)
  
  -- Add type annotation to clarify
  result <- (runAction config (processor inputs) :: IO (Either Error [Result]))
  
  -- Or use type application for parametric types
  result <- runAction @IO @Error config (processor inputs)
  ```

- **Testing File Paths in Different Environments**: Be aware of path separator differences:
  ```haskell
  -- In Windows, paths use backslashes
  -- In Unix-like systems, paths use forward slashes
  
  -- Use FilePath utilities for consistent handling
  let relPath = makeRelative root path
      -- Normalize for testing to handle platform differences
      normalizedPath = normalise relPath
      
  -- For testing, consider normalizing paths for comparison
  assertEqual "paths should match" 
    (normalise expectedPath) 
    (normalise actualPath)
  ```

## Tips

- Haskell has a sophisticated type system you can use to reason about code
- Haskell libraries tend to be high quality and, because of the nature of pure
  functions, you can easily copy and paste useful functions from third-party
  libraries rather than including entire libraries and encumbering yourself
  with dependencies
- Hoogle is a search engine that, among other things, can search by function
  type signature--this means if you need a function you can first figure out
  its type signature and then search Hoogle for an existing implementation
- Haskell's nature of using pure functions with well-defined
  mutation/uncertainty boundaries means that humans in the loop can focus on
  the uncertainty at those boundaries while you take care of the purely
  functional work more-or-less unattended.

## Advanced Type System Features

- **Constraint kinds**: When creating type aliases that represent constraints (like `type ErrorHandler r = Member (Error e) r`), always add the `ConstraintKinds` language extension.
- **Phantom types**: When working with phantom types like `TypedPath a`, use `Data.Coerce.coerce` for type conversions rather than complex pattern matching with GADT equality proofs.
- **Language extension coordination**: Advanced features require multiple extensions to work together properly:
  - Pattern synonyms need `PatternSynonyms` and often `ViewPatterns`
  - Type families need `TypeFamilies` and usually `UndecidableInstances`
  - GADTs need both `GADTs` and `DataKinds` for kind signatures
  - Deriving mechanisms need appropriate extensions (`DeriveGeneric`, `DerivingVia`, etc.)
  - Complex kind signatures need `KindSignatures`
  - When using effect systems, `FlexibleContexts` is almost always needed
- **Effect systems**: When using algebraic effects:
  - Every function using capability-based operations needs explicit effect constraints
  - Include `Member (Embed IO) r` in any function that calls into IO directly
  - Use `Members '[...]` syntax for multiple effect constraints
  - Consider alternatives to Polysemy for better type inference and error messages
  - For testing effects, prefer simpler effect stacks to avoid overlapping instances issues
- **Pattern synonyms**: Make pattern synonyms bidirectional when possible, providing both pattern and expression components for better usability and type inference.
- **Clean code practices**: Regularly remove unused imports, declarations, and type class instances as they create maintenance burden and compilation warnings.
- **Kleisli composition**: While elegant for monadic pipelines, sometimes direct do-notation is clearer, especially for complex error handling logic.
  - Use `import Control.Arrow ((>>>), (<<<), Kleisli(..), runKleisli)` when working with Kleisli arrows
  - Compose Kleisli arrows with `>>>` to create processing pipelines
  - Use `runKleisli` to execute the composed pipeline with an initial input
- **Type safety**: Prefer explicit types over type inference for public API functions to improve documentation and stability.
- **Type synonym constraints**: When working with type synonyms like `ClodM` in type signatures for higher-order functions (especially with Kleisli arrows), always check if the type synonym requires parameters. Many type errors occur because type synonyms aren't fully applied.
- **Graceful degradation**: When advanced type-level features cause compatibility issues, have fallback implementations ready that use simpler types but preserve the core logic.
- **Progressive enhancement**: Start with simple working code, then gradually introduce more sophisticated type-level abstractions. This helps isolate type errors and makes the code evolution more manageable.
- **Type-level programming benefits for human-AI collaboration**: 
  - Provides precise, machine-checkable specifications that guide AI implementations
  - Constrains solutions to match human intent through the type system
  - Makes domain concepts and invariants explicit, improving communication
  - Reduces the need for extensive documentation of invariants and constraints
- **ByteString handling**: When converting between ByteString and text data:
  - Use `map (toEnum . fromIntegral) . BS.unpack` when converting from ByteString to String
  - This handles Word8 to Char conversion properly with the intermediate fromIntegral step
  - For encoding, use `BS.pack . map (fromIntegral . fromEnum)` on String data

## Haddock Documentation

- **Template Haskell Generated Functions**: When using `makeSem` and similar TH functions that generate code:
  - Document the GADT constructors with Haddock comments before each constructor
  - For better documentation, create helper functions that re-export the generated functions with documentation
  - Example: `_readFileDoc :: Member FileSystem r => FilePath -> Sem r BS.ByteString; _readFileDoc = readFile`
  - For generated functions, add documentation in the `makeSem` call:
    ```haskell
    -- | Generate effect functions with Template Haskell
    -- Functions:
    -- 
    -- * 'readFile' :: Member FileSystem r => FilePath -> Sem r BS.ByteString
    -- * 'writeFile' :: Member FileSystem r => FilePath -> BS.ByteString -> Sem r ()
    makeSem ''FileSystem
    ```
  - Don't add separate type signatures for TH-generated functions, as it will cause "duplicate type signature" errors
  - Consider defining phantom documentation functions alongside TH-generated functions:
    ```haskell
    -- | Read a file from the file system
    -- Re-exported from FileSystem effect
    _readFileDoc :: Member FileSystem r => FilePath -> Sem r BS.ByteString
    _readFileDoc = readFile
    ```
  - These phantom documentation functions won't be exported but will show up in Haddock
  - Alternative approaches for better documentation:
    - Use the polysemy reexport pattern - create a module solely for reexporting with documentation
    - Document only the GADT constructors and explain in module docs that these correspond to functions

- **Qualification for Re-exported Functions**: When re-exporting functions from other modules:
  - Import the original module qualified (e.g., `import qualified Polysemy.Error as PE`)
  - Define local versions with proper documentation: `throw = PE.throw`
  - This avoids ambiguous references when the function is used elsewhere

- **Pattern Synonyms**: Always document pattern synonyms with Haddock comments, as they are part of your public API

- **Module Re-exports**: For functions re-exported from other modules, either:
  - Keep documentation in sync between the original and re-exported locations, or
  - Use `-- | @since x.y.z` to indicate version provenance and avoid duplicating documentation

## Working with Effect Systems

- **Import Qualification Strategy**: When using effect libraries like Polysemy:
  - Always qualify imports that define effect interpreters to avoid naming conflicts
  - Use consistent qualification prefixes across modules (e.g., `PE` for Polysemy.Error)
  - Be especially careful with functions like `throw`, `catch`, and `runError` that might appear in multiple modules

- **Consistent Error Handling**: When throwing errors in capability systems:
  - Define a unified approach to error construction across modules
  - Use qualified imports to avoid ambiguity in error-throwing functions

- **Type Annotations for Resolving Ambiguities**:
  - Add explicit type annotations when using functions with polymorphic return types
  - With Polysemy effects, use type applications for error types: `runError @MyErrorType`
  - When pattern matching on error types in case expressions, add type annotations to avoid ambiguity:
    ```haskell
    case result of
      Left (err :: MyErrorType) -> handleError err
      Right value -> pure value
    ```

- **Overlapping Instances Issues**:
  - Polysemy's Member instances can cause overlapping instance errors in complex effect stacks
  - When testing, simplify effect stacks to avoid these issues or use explicit type annotations
  - If all else fails, consider restructuring the code to use simpler effect systems or explicitly passing interpreters
  - For tests with overlapping instances, occasionally using separate test modules with different effect contexts can help

- **Debugging Complex Effect Stack Errors**:
  - Start with a minimal set of effects and add them one by one to isolate issues
  - Use explicit type annotations at each step to guide the type inference
  - Consider adding helper functions with restricted effect constraints for testing
  - If a specific effect combination causes persistent issues, consider alternative implementations or refactoring

## Advanced Functional Composition

- **Pipeline Pattern**: Use a list of effectful operations with a folding function to create processing pipelines:
  ```haskell
  processInSequence :: Monad m => [m (Either e a)] -> m (Either e a)
  processInSequence [] = pure $ Right defaultSuccess
  processInSequence (x:xs) = do
    result <- x
    case result of
      Left e -> pure $ Left e  -- Short-circuit on first error
      Right _ -> processInSequence xs  -- Continue processing
  ```

- **Kleisli Composition**: When composing functions that return monadic values, use Kleisli arrows from Control.Arrow:
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

- **Type Annotation for Generic Functions**: When writing highly generic composition helpers, specify concrete types in implementation:
  ```haskell
  -- This will cause ambiguity:
  processAll [] = pure $ Right successValue
  
  -- This is better:
  processAll [] = pure $ (Right successValue :: Either String Result)
  ```

## Cross-Platform Development

- **Pure Alternatives to Shell Commands**: Replace system commands with pure Haskell implementations:
  - Instead of using the `file` command to detect binary files, implement text detection with ByteString analysis
  - Check for null bytes, control character ratios, and UTF-8 validity
  - This improves cross-platform compatibility and removes external dependencies

- **Dynamic Binary Locating**: When writing wrapper scripts, use `find` to locate binaries rather than hardcoded paths:
  ```bash
  # Instead of hardcoded paths with specific architecture/version:
  # ./dist-newstyle/build/aarch64-osx/ghc-9.4.8/package-0.1.0/...
  
  # Use find to dynamically locate the binary:
  BINARY=$(find "./dist-newstyle" -name "package" -type f -executable | grep -v "\.dyn_o" | head -n 1)
  ```

- **Platform-Specific Code**: Explicitly document platform assumptions and provide graceful degradation:
  ```bash
  if [ "$(uname -s)" != "Darwin" ]; then
    echo "Warning: This feature works best on macOS. Continue? [y/N]"
    read -r response
    if [[ ! "$response" =~ ^[Yy] ]]; then
      echo "Operation canceled. Files are in: $OUTPUT_DIR"
      exit 0
    fi
  fi
  ```

## Security Best Practices

- **Canonical Paths in Error Messages**: When rejecting a path due to security restrictions:
  ```haskell
  -- Don't use the raw path in error messages
  throw $ SecurityError $ "Access denied: " ++ path
  
  -- Instead, use canonical paths for clarity and security
  canonicalPath <- embed $ canonicalizePath path
  throw $ SecurityError $ "Access denied: " ++ canonicalPath
  ```

- **Detailed Security Error Messages**: Provide specific reasons for access denials:
  ```haskell
  -- Instead of generic errors:
  throw $ SecurityError "Access denied"
  
  -- Use more specific errors with context:
  throw $ SecurityError $ "Access denied: Source path violates restrictions: " ++ srcPath
  ```

- **Reducing Duplicated Security Logic**: Use higher-order functions to encapsulate security patterns:
  ```haskell
  -- Create a single policy-checking function
  makePathPolicy :: [FilePath] -> FilePath -> Sem r Bool
  makePathPolicy allowedDirs path = embed $ isPathAllowed allowedDirs path
  
  -- Reuse it in multiple capability constructors
  fileReadCap dirs = FileReadCap { allowedDirs = dirs, policy = makePathPolicy dirs }
  fileWriteCap dirs = FileWriteCap { allowedDirs = dirs, policy = makePathPolicy dirs }
  ```

## JSON Generation and File Manifest Handling

- **Functional JSON Generation**: When generating JSON files like manifest files, use a purely functional approach:
  ```haskell
  -- Instead of incrementally building JSON with appendFile:
  addEntry :: FilePath -> Entry -> IORef Bool -> IO ()
  addEntry file entry isFirstRef = do
    isFirst <- readIORef isFirstRef
    unless isFirst $
      appendFile file ",\n"
    writeIORef isFirstRef False
    appendFile file (formatEntry entry)

  -- Use a more functional approach that collects entries first:
  writeJsonFile :: FilePath -> [Entry] -> IO ()
  writeJsonFile file entries = do
    let formattedEntries = zipWith formatEntry [0..] entries
        json = "{\n" ++ intercalate ",\n" formattedEntries ++ "\n}"
    writeFile file json
    
    -- Format a single entry with comma handling
    formatEntry :: Int -> Entry -> String
    formatEntry idx entry =
      let comma = if idx == length entries - 1 then "" else ","
      in "  \"" ++ key entry ++ "\": \"" ++ value entry ++ "\"" ++ comma
  ```

- **Avoiding File Locking Issues**: When reading and writing the same file, ensure proper handle closure:
  ```haskell
  -- Using strict IO to ensure file handles are closed promptly
  readFileStrict :: FilePath -> IO String
  readFileStrict path = do
    contents <- readFile path
    length contents `seq` return contents
    
  -- Use withFile for guaranteed handle closure
  withFileStrict :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
  withFileStrict path mode action = 
    withFile path mode $ \h -> do
      result <- action h
      hFlush h
      return result
  ```

- **Deduplication**: Use `nubBy` with a custom equality function for more sophisticated deduplication:
  ```haskell
  -- Remove duplicates based on a specific field
  import Data.List (nubBy)
  
  -- Deduplicate entries by a specific field
  uniqueEntries :: [Entry] -> [Entry]
  uniqueEntries = nubBy (\a b -> entryKey a == entryKey b)
  ```

- **Data Modeling**: Use newtype wrappers and records for JSON data:
  ```haskell
  -- Define clear types for manifest entries
  data ManifestEntry = ManifestEntry 
    { entryOptimizedName :: OptimizedName
    , entryOriginalPath :: OriginalPath 
    } deriving (Show, Eq)
    
  -- Use these in your JSON generation functions
  writeManifestFile :: FilePath -> [ManifestEntry] -> IO ()
  ```

- **JSON String Escaping**: Implement proper string escaping for JSON:
  ```haskell
  -- Escape special characters in JSON strings
  escapeJSON :: String -> String
  escapeJSON = concatMap escapeChar
    where
      escapeChar '\\' = "\\\\"
      escapeChar '"'  = "\\\""
      escapeChar c    = [c]
  ```

- **Incremental vs. Complete Rewrite**: For manifest files that may be updated incrementally:
  ```haskell
  -- Two-step approach - read then write completely new content
  processFiles config manifestPath files = do
    -- First collect all valid entries
    fileResults <- mapM processOneFile files
    
    -- Extract successful entries
    let newEntries = concatMap (maybe [] id . fst) fileResults
    
    -- Read any existing entries from the manifest, if it exists
    existingEntries <- readManifestEntries manifestPath
    
    -- Combine existing and new entries, then deduplicate
    let allEntries = existingEntries ++ newEntries
        uniqueEntries = nubBy (\a b -> entryOriginalPath a == entryOriginalPath b) allEntries
    
    -- Write all entries to the manifest file at once
    writeManifestFile manifestPath uniqueEntries
  ```

- **Simple Safe Parsing**: For quick-and-dirty parsing of simple formats:
  ```haskell
  -- Simple parsing to extract entries (basic approach)
  parseEntry line =
    case break (== ':') line of
      (optimizedPart, ':':restPart) -> do
        let chars = " \"," 
            cleanOptimized = filter (\c -> not (c `elem` chars)) optimizedPart
            cleanOriginal = filter (\c -> not (c `elem` chars)) restPart
        if null cleanOptimized || null cleanOriginal
          then Nothing
          else Just $ ManifestEntry 
                   (OptimizedName cleanOptimized) 
                   (OriginalPath cleanOriginal)
      _ -> Nothing
  ```

- **Memory-Efficient Processing**: When processing many files:
  ```haskell
  -- Two-step processing for larger file sets
  processFiles config manifestPath files includeInManifestOnly = do
    -- First pass - collect entries just for manifest
    manifestEntries <- if includeInManifestOnly
                      then collectManifestEntries files
                      else collectAndCopyFiles files
                      
    -- Write entries to manifest file, deduplicating as needed
    writeManifestFile manifestPath manifestEntries
  ```

- **Caching Entry Results**: For expensive operations:
  ```haskell
  -- Cache file processing results
  let processWithCache path = 
        case Map.lookup path cache of
          Just result -> return result
          Nothing -> do
            result <- processFile path
            let newCache = Map.insert path result cache
            return (newCache, result)
  ```

## Ignore Pattern Handling

- **Hierarchical Pattern Structure**: When implementing ignore patterns like .gitignore or .clodignore:
  ```haskell
  -- Define a clear type for ignore patterns
  newtype IgnorePattern = IgnorePattern String deriving (Show, Eq)
  
  -- Categorize patterns into inclusion and negation types
  categorizePatterns :: [IgnorePattern] -> ([IgnorePattern], [IgnorePattern])
  categorizePatterns = L.partition isInclusion
    where 
      isInclusion (IgnorePattern p) = not ("!" `L.isPrefixOf` p)
  ```

- **Pattern Matching**: Implement robust pattern matching for ignore patterns:
  ```haskell
  -- Pattern matcher for different types of patterns
  matchesIgnorePattern :: [IgnorePattern] -> FilePath -> Bool
  matchesIgnorePattern patterns filePath = 
    -- Process patterns in reverse order (later patterns take precedence)
    let (inclusions, negations) = categorizePatterns patterns
        
        -- Process negation patterns - extract the pattern without '!'
        negationPatterns = map (\(IgnorePattern p) -> 
                            IgnorePattern (drop 1 p)) negations
        
        -- Check if any inclusion pattern matches
        includedByPattern = any (matchesPattern filePath) inclusions
        
        -- Check if any negation pattern matches
        negatedByPattern = any (matchesPattern filePath) negationPatterns
    in
      -- Included by some pattern and not negated by any later pattern
      includedByPattern && not negatedByPattern
  ```

- **Loading Multiple Ignore Files**: Support loading ignore patterns from multiple sources:
  ```haskell
  -- Load patterns from .gitignore and .clodignore
  loadIgnorePatterns :: FilePath -> ClodM [IgnorePattern]
  loadIgnorePatterns projectPath = do
    gitIgnorePatterns <- readGitIgnore projectPath
    clodIgnorePatterns <- readClodIgnore projectPath
    let allPatterns = gitIgnorePatterns ++ clodIgnorePatterns
    return allPatterns
  ```

- **Applying Ignore Patterns**: Apply ignore patterns consistently in file processing:
  ```haskell
  -- Check if a file should be ignored based on patterns
  shouldBeIgnored :: [IgnorePattern] -> FilePath -> Bool
  shouldBeIgnored patterns relPath = 
    not (null patterns) && matchesIgnorePattern patterns relPath
    
  -- Use in processing pipeline
  processFile readCap writeCap fullPath relPath = do
    let steps = [ checkIgnorePatterns fullPath relPath
                , checkFileExists readCap fullPath relPath
                , checkIsTextFile readCap fullPath relPath
                , copyToStaging readCap writeCap fullPath relPath
                ]
  ```

- **Glob Pattern Handling**: Implement efficient matching for glob patterns:
  ```haskell
  -- Match glob patterns like "*.js" or "src/**/*.ts"
  simpleGlobMatch :: String -> FilePath -> Bool
  simpleGlobMatch pattern path = matchGlob pattern path
    where
      matchGlob ('*':'*':'/':rest) (c:cs) -- Directory wildcard: **/
        | c == '/' = matchGlob ('*':'*':'/':rest) cs
        | otherwise = matchGlob ('*':'*':'/':rest) (c:cs) || matchGlob rest (c:cs)
      
      matchGlob ('*':'.':ext) path -- File extension: *.ext
        | takeExtension path == '.' : ext = True
        | otherwise = False
  ```

- **Testing Ignore Patterns**: Create robust tests for ignore pattern functionality:
  ```haskell
  it "respects ignore patterns" $ do
    withSystemTempDirectory "test-dir" $ \tmpDir -> do
      -- Setup test files
      createDirectoryIfMissing True (tmpDir </> "src")
      createDirectoryIfMissing True (tmpDir </> "node_modules")
      
      -- Create ignore pattern files
      writeFile (tmpDir </> ".gitignore") "node_modules/\n"
      
      -- Run application
      result <- runApp tmpDir
      
      -- Check for processed files
      srcFileExists <- doesFileExist (tmpDir </> "output" </> "src-file.txt")
      nodeModuleFileExists <- doesFileExist (tmpDir </> "output" </> "node_modules-file.txt")
      
      -- Verify patterns were properly applied
      srcFileExists `shouldBe` True
      nodeModuleFileExists `shouldBe` False
  ```

## Path Handling and Filename Transformations

- **Filename Sanitization**: When sanitizing filenames, be careful with extensions and dots:
  ```haskell
  -- Naive approach that might cause issues:
  sanitizeFilename = filter (`notElem` "./\\:*?\"<>|")
  
  -- Better approach that preserves dots in filenames:
  sanitizeFilename = map (\c -> if c `elem` "/\\:*?\"<>|" then '_' else c)
  ```

- **Path Normalization for Git Operations**: When comparing paths in Git operations:
  ```haskell
  -- Normalize paths from LibGit and generated paths for comparison
  normalizePath :: FilePath -> FilePath
  normalizePath = dropDrive . normalise
  
  -- When comparing Git paths with local paths:
  libgitPath `isPathMatch` localPath = 
    normalizePath libgitPath == normalizePath localPath
  ```

- **Path Directory Validation**: When checking if a path is within allowed directories:
  ```haskell
  -- Check if a path is within allowed directories
  isAllowedPath :: [FilePath] -> FilePath -> IO Bool
  isAllowedPath allowedDirs path = do
    canonicalPath <- canonicalizePath path
    let normalizedPath = normalise canonicalPath
    return $ any (\dir -> normalizedPath `isSubdirectoryOf` dir) allowedDirs
    
  -- Helper to check if path is subdirectory
  isSubdirectoryOf :: FilePath -> FilePath -> Bool
  path `isSubdirectoryOf` dir = 
    normalise dir `isPrefixOf` normalise path && 
    (null (normalise dir) || normalise path /= normalise dir)
  ```

- **Avoiding Unwanted Path Prefixes**: When traversing filesystem directories to collect files:
  ```haskell
  -- Special handling for root directory traversal to avoid "./" prefixes
  findFilesRecursive :: FilePath -> ClodM [FilePath]
  findFilesRecursive file = do
    -- Special case for empty string or "." to handle root directory
    let useBasePath = null file || file == "."
        fullPath = if useBasePath then basePath else basePath </> file
    
    isDir <- liftIO $ doesDirectoryExist fullPath
    case isDir of
      False -> return [file]  
      True  -> do
        -- Get directory contents, excluding "." and ".."
        contents <- liftIO $ getDirectoryContents fullPath
        let validContents = filter (`notElem` [".", ".."]) contents
        
        -- Recursively process subdirectories
        subFiles <- findAllFiles fullPath validContents
        
        -- Add directory prefix only if not at root
        return $ if useBasePath
                then subFiles  -- For root dir, don't add any prefix
                else map (file </>) subFiles
  ```

- **Handling Hidden Files**: When transforming filenames, handle hidden files (dot files) specially:
  ```haskell
  -- Transform file names according to spec
  transformFilename :: String -> String -> String
  transformFilename name original
    -- Handle hidden files (those starting with a dot)
    | not (null name) && head name == '.' =
        -- Remove the leading dot and add the "dot--" prefix
        "dot--" ++ tail name
    -- Other special cases...
    | otherwise = name
    
  -- When flattening directory paths with hidden directories  
  transformDirPart :: String -> String
  transformDirPart dir = 
    -- Remove trailing slash if present
    let cleanDir = if L.isSuffixOf "/" dir then init dir else dir
    -- Apply hidden file transformation if needed
    in if not (null cleanDir) && head cleanDir == '.'
       then "dot--" ++ tail cleanDir
       else cleanDir
  ```

- **Directory Path Segmentation**: When splitting paths into segments, handle directory separators correctly:
  ```haskell
  -- Split a path into its directory components
  splitPath :: FilePath -> [String]
  splitPath path = filter (not . null) $ map getSegment $ L.groupBy sameGroup path
    where
      sameGroup c1 c2 = c1 /= '/' && c2 /= '/'
      getSegment seg = filter (/= '/') seg
  ```

- **Preserving Path Structure**: When transforming files while preserving path structure:
  ```haskell
  -- Create output paths that mirror input structure
  makeOutputPath :: FilePath -> FilePath -> FilePath -> FilePath
  makeOutputPath srcRoot destRoot relPath =
    destRoot </> relPath
  
  -- Calculate relative path correctly
  getRelativePath :: FilePath -> FilePath -> FilePath
  getRelativePath base path = 
    makeRelative (normalise base) (normalise path)
  ```

## Binary File Detection

- **Externalized Configuration with Dhall**: Use Dhall for structured, type-safe configuration of file types:
  ```haskell
  -- Define data types for Dhall configuration
  data FileTypes = FileTypes
    { textExtensions :: [String]
    , binaryExtensions :: [String]
    , textSpecialCases :: [String]
    , binarySpecialCases :: [String]
    } deriving (Show, Generic)
  
  instance FromDhall FileTypes
  instance ToDhall FileTypes
  
  -- Define data types for binary signatures
  data BinarySignature = BinarySignature
    { name :: String
    , bytes :: [Word8]
    } deriving (Show, Generic)
  
  instance FromDhall BinarySignature
  
  -- Container for binary signatures
  newtype BinarySignatures = BinarySignatures
    { signatures :: [BinarySignature]
    } deriving (Show, Generic)
  
  instance FromDhall BinarySignatures
  
  -- Load configuration from Dhall files
  loadFileTypes :: IO FileTypes
  loadFileTypes = do
    path <- getDataFileName "resources/file_types.dhall"
    (input auto (T.pack path) :: IO FileTypes) `catch` \(_ :: SomeException) -> 
      pure defaultFileTypes
  ```

- **Robust Binary Detection**: Combine multiple strategies for reliable binary file detection:
  ```haskell
  -- Use multiple detection strategies for reliable results
  isTextContent :: FilePath -> BS.ByteString -> Bool
  isTextContent file content = unsafePerformIO $ do
    -- Load file types and binary signatures
    fileTypes <- loadFileTypes
    signatures <- loadBinarySignatures
    return $ isTextContentPure file content fileTypes signatures
  
  isTextContentPure :: FilePath -> BS.ByteString -> FileTypes -> BinarySignatures -> Bool
  isTextContentPure file content fileTypes binarySigs = 
    let 
      -- Take a sample from the beginning of the file
      sample = BS.take sampleSize content
      
      -- Apply detection strategies
      isText = isEmpty || 
               (isTextExt && not hasNullByte) || 
               (not hasSignature && 
                not hasNullByte && 
                isValidUtf8 && 
                controlCharRatio < 0.2 && 
                not isBinaryExt)
    in isText
  ```

- **File Signatures**: Check for magic bytes that identify common binary formats:
  ```haskell
  -- Check if a byte sequence matches a binary file signature
  matchesSignature :: BS.ByteString -> [Word8] -> Bool
  matchesSignature bs sig
    | BS.length bs < length sig = False
    | otherwise = BS.take (length sig) bs == BS.pack sig
  ```

- **UTF-8 Validation**: Validate the UTF-8 encoding of text files:
  ```haskell
  -- Simple UTF-8 validation for multi-byte sequences
  validateUtf8 :: BS.ByteString -> Bool
  validateUtf8 bytes = validateBytes (BS.unpack bytes) 0
    where
      validateBytes [] _ = True
      validateBytes (b:rest) 0
        | b < 128 = validateBytes rest 0                -- ASCII
        | b .&. 0xE0 == 0xC0 = validateBytes rest 1     -- 2-byte
        | b .&. 0xF0 == 0xE0 = validateBytes rest 2     -- 3-byte
        | b .&. 0xF8 == 0xF0 = validateBytes rest 3     -- 4-byte
        | otherwise = False                             -- Invalid
      validateBytes (b:rest) n
        | b .&. 0xC0 == 0x80 = validateBytes rest (n-1) -- Valid continuation
        | otherwise = False                             -- Invalid continuation
  ```

- **Control Character Analysis**: Analyze the distribution of control characters:
  ```haskell
  -- Check if a byte is a control or non-printable character
  isControlNonPrintChar :: Word8 -> Bool
  isControlNonPrintChar b = 
    -- Control chars below 32, except common whitespace
    (b < 32 && b /= 9 && b /= 10 && b /= 13) || 
    -- DEL (127)
    b == 127 ||
    -- Invalid UTF-8 sequences
    (b >= 128 && b < 192)
    
  -- Calculate the ratio of control characters
  controlCharCount = BS.length $ BS.filter isControlNonPrintChar sample
  controlCharRatio = fromIntegral controlCharCount / fromIntegral totalBytes
  ```

- **Null Byte Policy**: Treat null bytes as a strong indicator of binary content:
  ```haskell
  -- Files with null bytes should always be considered binary,
  -- even if they have a text extension
  isText = isEmpty || (isTextExt && not hasNullByte) || ...
  ```

## External Configuration with Dhall

- **Type-safe Configuration**: Use Dhall for configuration to get type safety and better error reporting:
  ```haskell
  -- Define data types with appropriate instances
  data MyConfig = MyConfig
    { field1 :: Int
    , field2 :: [String]
    } deriving (Show, Generic)
  
  instance FromDhall MyConfig
  instance ToDhall MyConfig
  
  -- Load configuration
  loadConfig :: IO MyConfig
  loadConfig = do
    path <- getDataFileName "resources/config.dhall"
    (input auto (T.pack path) :: IO MyConfig) `catch` \(_ :: SomeException) -> 
      pure defaultConfig
  ```

- **Default Values for Resilience**: Always provide default values for configurations:
  ```haskell
  -- Default configuration when the file cannot be loaded
  defaultConfig :: MyConfig
  defaultConfig = MyConfig
    { field1 = 42
    , field2 = ["default", "values"]
    }
  ```

- **Dhall Structure for Lists**: Define lists in Dhall in a clean, readable format:
  ```dhall
  -- In your Dhall file (e.g., file_types.dhall)
  { 
    textExtensions = 
      [ -- Documentation
        ".txt", ".text", ".md", ".markdown", ".csv", ".tsv"
        -- Markup  
      , ".html", ".htm", ".xhtml", ".xml", ".svg", ".rss"
      ]
  }
  ```

- **Records and Custom Types**: Define custom types in Dhall:
  ```dhall
  -- Define a type for binary signatures
  let Signature = { name : Text, bytes : List Natural }
  
  let signatures : List Signature =
      [ { name = "JPEG", bytes = [0xFF, 0xD8, 0xFF] }
      , { name = "PNG", bytes = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A] }
      ]
  
  in { signatures = signatures }
  ```

- **Text Conversion for FilePath**: Dhall's `input` function requires Text but FilePath is String:
  ```haskell
  import qualified Data.Text as T
  
  loadDhallConfig :: IO Config
  loadDhallConfig = do
    path <- getDataFileName "resources/config.dhall"
    -- Convert FilePath (String) to Text for Dhall input
    (input auto (T.pack path) :: IO Config) `catch` \(_ :: SomeException) -> 
      pure defaultConfig
  ```

- **Error Handling with Default Values**: Use `catch` to handle any Dhall parsing errors:
  ```haskell
  import Control.Exception (SomeException, catch)
  
  -- Gracefully handle any parsing errors by using defaults
  loadConfig :: IO Config
  loadConfig = do
    path <- getDataFileName "resources/config.dhall"
    (input auto (T.pack path) :: IO Config) `catch` \(_ :: SomeException) -> do
      putStrLn "Warning: Could not load config, using defaults"
      pure defaultConfig
  ```

- **Avoiding Pure Unsafety**: Use careful application of `unsafePerformIO` for configuration loading:
  ```haskell
  -- This is safer than it looks because configuration loading is idempotent
  -- and the result is referentially transparent (same input always yields same output)
  getConfig :: Config
  getConfig = unsafePerformIO $ do
    -- Cache this result with NOINLINE pragma
    loadConfig
  {-# NOINLINE getConfig #-}
  ```
  
- **Type-Safe Field Access**: Access record fields directly with qualified record syntax:
  ```haskell
  -- When using a record from Dhall in a function
  isTextFile :: FilePath -> Config -> Bool
  isTextFile path config =
    let ext = takeExtension path
    in ext `elem` textExtensions config
  ```

- **Data Files Registration**: Register Dhall files as data-files in cabal file:
  ```
  data-files:
    resources/config.dhall,
    resources/file_types.dhall
  ```
