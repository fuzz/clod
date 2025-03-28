# Changelog for Clod

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-03-27
### Added
- Integrated libgit2 for native Git operations via hlibgit2 bindings
- Improved pattern matching with negation (!pattern) support
- Added character class matching support ([a-z], [0-9])
- Implemented proper pattern precedence handling (like Git)
- Added nested .gitignore file support
- Implemented pattern caching for performance improvements

## [0.1.0] - 2025-03-27
### Added
- Initial release with core functionality
- Support for tracking modified files
- Respect for .gitignore and .clodignore patterns
- Automatic creation of default .clodignore file
- Case-insensitive extension matching
- Optimized file naming for Claude AI integration
- Path manifest generation for mapping files
- Comprehensive documentation with Haddock
- Test suite with unit and property-based tests
- Hackage package revision with improved code quality

### Changed
- Configuration directory renamed from ".claude-uploader" to ".clod"
- Removed verbose "Skipped" output messages for cleaner interface
- Refactored pattern matching to use higher-order functions
- Enhanced glob matching with function composition
- Improved file type transformation with cleaner pattern matching
- Added comments about potential ReaderT pattern for future extension

### Fixed
- Fixed SVG pattern matching bug with case-insensitive extension matching
- Improved pattern matching for wildcard extensions
- Fixed handling of directory patterns with trailing slashes
