# Changelog for Clod

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.1] - 2025-03-31
### Changed
- Removed unused dependencies (polysemy, deepseq, lens)
- Unified file format for path manifest to use Dhall instead of JSON for consistency

## [0.1.0] - 2025-03-31
### Added
- Initial release with core functionality
- Checksum-based file tracking to detect modified files
- Support for efficient file change detection using XXH3 (64-bit) hashes
- Database of file checksums for tracking changes between runs
- Rename detection using content checksums
- Magic-based file type detection using libmagic
- Respect for .gitignore and .clodignore patterns
- Automatic creation of default .clodignore file
- Case-insensitive extension matching
- Optimized file naming for Claude AI integration
- _path_manifest.dhall generation for mapping optimized filenames back to original paths
- Command-line options:
  - --all: Import all files (respecting .gitignore)
  - --test: Run in test mode
  - --staging-dir: Specify a custom staging directory for test mode
  - --verbose: Enable verbose output
  - --flush: Remove missing entries from the checksum database
  - --last: Reuse the previous staging directory
- Capability-based permission model for file system operations
- Path-restricted file operations to prevent unauthorized access
- Nested .gitignore file support
- Pattern caching for performance improvements
- Traditional monad stack (ReaderT/ExceptT/IO) for better type inference and clearer error messages
- Comprehensive man pages (clod.1, clod.7, clod.8) installed to standard system locations
- Cross-platform support (macOS, Linux, Windows)
- Homebrew formula template for macOS distribution
- Dhall serialization for configurations

### Future Plans
- Performance optimizations for large codebases
- Parallel file processing capabilities
- Enhanced file transformations based on file type
- Integration with cloud storage services
- Remote file processing capabilities
- Optional GUI interface