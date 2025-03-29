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

## Algebraic Effects and Capability-Based Security

- Use algebraic effects libraries like `polysemy` to make side effects explicit at the type level.
- Implement capability-based security for operations with potential security implications:
  - Define a capability as a token that grants specific permissions (e.g., read access to specific directories).
  - Require these tokens as parameters for any potentially unsafe operations.
  - Check permissions at runtime before performing the operation.
- Make effect handlers composable and modular to enable selective capabilities.
- Design test cases specifically to verify that capability restrictions work correctly.
- For file system operations, use path prefix checking to restrict access to allowed directories.
- Use the containing directory structure for temporary outputs to maintain clean repository state.
- Add output directories to .gitignore to avoid committing generated artifacts.

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
- **Effect systems**: When using algebraic effects:
  - Every function using capability-based operations needs explicit effect constraints
  - Include `Member (Embed IO) r` in any function that calls into IO directly
  - Use `Members '[...]` syntax for multiple effect constraints
  - Consider alternatives to Polysemy for better type inference and error messages
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

## Haddock Documentation

- **Template Haskell Generated Functions**: When using `makeSem` and similar TH functions that generate code:
  - Document the GADT constructors with Haddock comments before each constructor
  - For better documentation, create helper functions that re-export the generated functions with documentation
  - Example: `_readFileDoc :: Member FileSystem r => FilePath -> Sem r BS.ByteString; _readFileDoc = readFile`

- **Qualification for Re-exported Functions**: When re-exporting functions from other modules:
  - Import the original module qualified (e.g., `import qualified Polysemy.Error as PE`)
  - Define local versions with proper documentation: `throw = PE.throw`
  - This avoids ambiguous references when the function is used elsewhere

- **Pattern Synonyms**: Always document pattern synonyms with Haddock comments, as they are part of your public API

## Working with Effect Systems

- **Import Qualification Strategy**: When using effect libraries like Polysemy:
  - Always qualify imports that define effect interpreters to avoid naming conflicts
  - Use consistent qualification prefixes across modules (e.g., `PE` for Polysemy.Error)
  - Be especially careful with functions like `throw`, `catch`, and `runError` that might appear in multiple modules

- **Consistent Error Handling**: When throwing errors in capability systems:
  - Define a unified approach to error construction across modules
  - Use qualified imports to avoid ambiguity in error-throwing functions

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
