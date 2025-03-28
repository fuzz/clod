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
