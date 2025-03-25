# Clod - Claude Loader

[![Hackage](https://img.shields.io/hackage/v/clod.svg)](https://hackage.haskell.org/package/clod)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge feature. It tracks file changes, respects `.gitignore` and `.clodignore` patterns, and optimizes filenames for Claude's UI.

## Features

- Track modified files since last run
- Respect `.gitignore` and `.clodignore` patterns
- Handle binary vs. text files automatically
- Create optimized filenames for Claude's UI
- Generate a path manifest for mapping optimized names back to original paths
- Color-coded, user-friendly terminal interface

## Installation

### From Hackage

```bash
cabal install clod
```

### From Source

```bash
git clone https://github.com/yourusername/clod.git
cd clod
cabal install
```

### Prerequisites

- GHC (Glasgow Haskell Compiler) 9.0 or newer
- Git (required for repository operations)
- macOS (primary platform, other platforms supported with limitations)

## Usage

### Basic Usage

```bash
# Process modified files since last run
clod

# Process all files (respecting .gitignore and .clodignore)
clod --all

# Specify a custom staging directory
clod --staging-dir /path/to/staging
```

### Command-Line Options

- `--all`: Process all files, not just modified ones
- `--modified`: Process only modified files (default)
- `--staging-dir DIR`: Specify a custom staging directory
- `--test`: Run in test mode (no prompts, useful for CI)
- `--help`: Show help information
- `--version`: Show version information

### First Run

On first run, Clod will:

1. Ask for a staging directory (defaults to `~/Claude`)
2. Create a default `.clodignore` file if one doesn't exist
3. Prompt you to choose which files to process:
   - All files
   - Only modified files
   - None (just set timestamp)

### Integration with Claude AI

After running Clod:

1. Navigate to Project Knowledge in your Claude Project (Pro or Team account required)
2. Drag files from the staging folder to Project Knowledge
3. Include the `_path_manifest.json` file which maps optimized names back to original paths
4. Paste the contents of `project-instructions.md` into the Project Instructions section
5. **Important**: You must manually delete previous versions of these files from Project Knowledge before starting a new conversation to ensure Claude uses the most recent files

## Configuration

### .clodignore

Create a `.clodignore` file in your repository root to specify files or patterns to exclude:

```
# Binary and media files
*.dll
*.dylib
*.exe
*.gif
*.ico
*.jar
*.jpg
*.jpeg
*.mp3
*.mp4
*.png
*.so
*.svg
*.tar.gz
*.zip

# Build directories
.clod
.git
build
dist
node_modules
out
target

# Large files and lock files
*.log
Cargo.lock
package-lock.json
pnpm-lock.yaml
yarn.lock
```

## Project Structure

- `app/`: Application entry point
- `src/`: Source code modules
  - `Clod/Core.hs`: Main functionality
  - `Clod/FileSystem.hs`: File operations
  - `Clod/Git.hs`: Git integration
  - `Clod/IgnorePatterns.hs`: Pattern matching
  - `Clod/Output.hs`: User interface
  - `Clod/Types.hs`: Core types
- `test/`: Test suite
- `.clod/`: Configuration and state (created during execution)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details.

### Development Workflow

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for your changes
5. Run the test suite with `cabal test`
6. Submit a pull request

### Release Process

See [RELEASING.md](RELEASING.md) for details on the release process.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Claude AI team for the Project Knowledge feature
- Haskell community for their excellent libraries