# Clod - Claude Loader

[![Hackage](https://img.shields.io/hackage/v/clod.svg)](https://hackage.haskell.org/package/clod)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge feature. It tracks file changes, respects `.gitignore` and `.clodignore` patterns, and optimizes filenames for Claude's UI. By efficiently handling file selection and staging, it can significantly reduce AI development costs by 50% or more.

See [HUMAN.md](HUMAN.md) for a complete workflow guide to using `clod` with Claude AI.

For information about the type-safe effects system and capability-based security model, see [EFFECTS.md](EFFECTS.md).

## Features

- Track modified files since last run
- Respect `.gitignore` and `.clodignore` patterns
- Handle binary vs. text files automatically
- Use system temporary directories for staging files
- Create optimized filenames for Claude's UI
- Generate a path manifest for mapping optimized names back to original paths
- Color-coded, user-friendly terminal interface
- Capability-based security for file operations
- Type-safe effects system to enhance code safety

## Installation

### From Hackage

```bash
cabal install clod
```

### From Source

```bash
git clone https://github.com/fuzz/clod.git
cd clod
cabal install
```

For detailed installation instructions, including how to install man pages, see [INSTALLING.md](INSTALLING.md).

After installing, run the provided script to install the man pages to your user directory:

```bash
# Clone the repository 
git clone https://github.com/fuzz/clod.git
cd clod

# Install the man pages
./bin/install-man-pages.sh
```

This will make the documentation accessible via `man clod`.

### Prerequisites

- GHC (Glasgow Haskell Compiler) 9.0 or newer
- libgit2 (required for Git operations)
  - On macOS: `brew install libgit2`
  - On Linux: `apt-get install libgit2-dev` or equivalent for your distribution
  - On Windows: Install from source or use package manager

**Note:** While clod is designed to be cross-platform, it has primarily been tested on macOS. The tool uses the `open` command to display the staging directory, which is natively available on macOS. Users on other platforms may need to:

* Create an `open` command or alias that opens a file browser for a given directory path
* Use the displayed staging directory path to manually open the files

Pull requests for improved cross-platform support are welcome.

## Usage

### Basic Usage

```bash
# Process modified files since last run
clod

# Process all files (respecting .gitignore and .clodignore)
clod --all

# Run in test mode with an optional test directory
clod --test
```

### Command-Line Options

- `--all`: Process all files, not just modified ones
- `--modified`: Process only modified files (default)
- `--test`: Run in test mode (no prompts, useful for CI)
- `--staging-dir DIR`: Specify a directory for test mode (only used with --test)
- `--help`: Show help information
- `--version`: Show version information

### First Run

On first run, Clod will:

1. Create a system temporary directory for staging files
2. Create a default `.clodignore` file if one doesn't exist
3. Prompt you to choose which files to process:
   - All files
   - Only modified files
   - None (just set timestamp)

### Integration with Claude AI

After running Clod:

1. Navigate to Project Knowledge in your Claude Project (Pro or Team account required)
2. Drag files from the opened staging folder to Project Knowledge
3. Include the `_path_manifest.json` file which maps optimized names back to original paths
4. Paste the contents of `project-instructions.md` into the Project Instructions section
5. **Important**: You must manually delete previous versions of these files from Project Knowledge before starting a new conversation to ensure Claude uses the most recent files
6. Note that the staging directory is temporary and will be cleaned up on your next run of clod (or system reboot)

## Configuration

### Environment Variables

You can customize Clod's behavior using these environment variables:

- `CLOD_DIR` - Override the default `.clod` directory name
- `CLODIGNORE` - Override the default `.clodignore` filename

Example:
```bash
# Use custom locations 
CLOD_DIR=.config/clod CLODIGNORE=.config/clod/ignore clod
```

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
  - `Clod/Config.hs`: Environment and configuration handling
  - `Clod/Core.hs`: Main functionality
  - `Clod/FileSystem.hs`: File operations facade
    - `Clod/FileSystem/Detection.hs`: File type detection
    - `Clod/FileSystem/Operations.hs`: Basic file operations
    - `Clod/FileSystem/Processing.hs`: File processing and manifest generation
    - `Clod/FileSystem/Transformations.hs`: Special file format transformations
  - `Clod/Git.hs`: Git integration facade
    - `Clod/Git/Repository.hs`: Repository operations
    - `Clod/Git/Status.hs`: Status checking operations
    - `Clod/Git/Internal.hs`: Internal file processing functions
    - `Clod/Git/LibGit.hs`: libgit2 integration using hlibgit2
  - `Clod/IgnorePatterns.hs`: Pattern matching
  - `Clod/Output.hs`: User interface
  - `Clod/Types.hs`: Core types and monad stack
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
