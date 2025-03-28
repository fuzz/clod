# Changelog for Clod

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-03-27
### Added
- Initial release with core functionality
- Support for tracking modified files
- Respect for .gitignore and .clodignore patterns
- Automatic creation of default .clodignore file
- Case-insensitive extension matching
- Optimized file naming for Claude AI integration
- Path manifest generation for mapping files
- Integrated libgit2 for native Git operations via hlibgit2 bindings
- Effects system using polysemy for improved type safety and AI security
- Capability-based permission model for file system operations
- Path-restricted file operations to prevent unauthorized access
- Nested .gitignore file support
- Pattern caching for performance improvements