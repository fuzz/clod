# Changelog

## [0.1.18] - 2025-04-03

Test release process again

## [0.1.17] - 2025-04-03

Update README, remove crufty documentation files, test release again

## [0.1.16] - 2025-04-03

Run the tests before the rest of the release process

## [0.1.15] - 2025-04-03

Another version bump to test the full release process

## [0.1.14] - 2025-04-03

Version bump to test new release script--now with binary distribution for macOS
Apple Silicon

## [0.1.13] - 2025-04-03

Update maintainer email

## [0.1.12] - 2025-04-01

Version bump to pick up script changes

## [0.1.7] - 2025-04-01

This entire project was created through human-AI pair programming, with Claude
as the sole code and documentation author and Fuzz providing guidance,
architectural decision, and final review.

## [0.1.6] - 2025-03-31
### Changed
- Simplified man page installation in Homebrew formula
- Improved cross-platform compatibility
- Removed complex installation scripts in favor of more standard approaches
- Enhanced Homebrew formula based on best practices

## [0.1.5] - 2025-03-31
### Changed
- Major improvements to man page installation script:
  - Added detailed logging to aid in debugging installation issues
  - Enhanced pandoc detection to work in multiple environment configurations
  - Ensured script gracefully handles missing files or tools
  - Added fallbacks for all error conditions to prevent build failures
  - Fixed path handling edge cases in installation process
- Updated Homebrew formula to better handle installation requirements

## [0.1.4] - 2025-03-31
### Changed
- Updated man pages with improved documentation
- Fixed version inconsistencies across project files
- Improved error handling for file path validation
- Enhanced hidden file transformation logic
- Documentation updates across the project

## [0.1.3] - 2025-03-31
### Changed
- Minor performance improvements for large codebases
- Enhanced error messages for better troubleshooting
- Fixed edge cases in path transformation for special characters
- Improved SVG to XML transformation reliability

## [0.1.2] - 2025-03-31
### Changed
- Updated Dhall dependency to support newer versions (< 1.43)
- Fixed compatibility issues with template-haskell 2.20.0.0

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
