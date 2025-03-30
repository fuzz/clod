# Changelog for Clod

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Checksum-based file tracking to detect modified files (replaces Git dependency)
- Support for efficient file change detection using SHA-256 hashes
- Database of file checksums for tracking changes between runs
- Rename detection using content checksums
- Magic-based file type detection using libmagic
- New flags: --flush and --last
  - --flush: Remove missing entries from the checksum database
  - --last: Reuse the previous staging directory
- Dhall serialization for the database configuration

### Changed
- Removed --modified flag as it's now the default behavior: first run processes all files, subsequent runs process only modified files (as specified in SPEC.md)
- Updated documentation to clarify the default behavior for file processing

## [0.1.0] - 2025-03-30
### Added
- Initial release with core functionality
- Support for tracking modified files
- Respect for .gitignore and .clodignore patterns
- Automatic creation of default .clodignore file
- Case-insensitive extension matching
- Optimized file naming for Claude AI integration
- Path manifest generation for mapping files
- Integrated libgit2 for native Git operations via hlibgit2 bindings
- Capability-based permission model for file system operations
- Path-restricted file operations to prevent unauthorized access
- Nested .gitignore file support
- Pattern caching for performance improvements
- Comprehensive man pages (clod.1, clod.7, clod.8) installed to standard system locations
- Shell script wrapper (`cld`) with cross-platform file browser integration
- Homebrew formula template for macOS distribution

### Changed
- Replaced Git integration with a pure Haskell checksum-based approach
- Removed dependency on libgit2 for a more portable, cross-platform solution
- Replaced Polysemy effects system with a traditional monad stack (ReaderT/ExceptT/IO) for better type inference and simpler error messages
- Preserved capability-based security model with runtime checks
- Simplified testing infrastructure by removing effect-specific test helpers
- Improved documentation with clearer examples that don't rely on effect-specific knowledge

### Future Plans
- Evaluating algebraic effects systems for a future version (1.0.0) with improved type inference and error messages
- Reintroduction of static type-level guarantees once a better effects solution is identified