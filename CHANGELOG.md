# Changelog for Clod

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-03-24
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

### Changed
- Configuration directory renamed from ".claude-uploader" to ".clod"
- Removed verbose "Skipped" output messages for cleaner interface

### Fixed
- Fixed SVG pattern matching bug with case-insensitive extension matching
- Improved pattern matching for wildcard extensions
- Fixed handling of directory patterns with trailing slashes
