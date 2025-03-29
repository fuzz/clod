# Clod - Claude Loader

[![Hackage](https://img.shields.io/hackage/v/clod.svg)](https://hackage.haskell.org/package/clod)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge feature. It tracks file changes, respects `.gitignore` and `.clodignore` patterns, and optimizes filenames for Claude's UI. By efficiently handling file selection and staging, it can significantly reduce AI development costs by 50% or more.

See [HUMAN.md](HUMAN.md) for a complete workflow guide to using `clod` with Claude AI.

For information about the capability-based security model, see [CAPABILITIES.md](CAPABILITIES.md).

## Features

- Track modified files since last run
- Respect `.gitignore` and `.clodignore` patterns
- Handle binary vs. text files automatically
- Use system temporary directories for staging files
- Create optimized filenames for Claude's UI
- Generate a path manifest for mapping optimized names back to original paths
- Color-coded, user-friendly terminal interface
- Capability-based security for file operations
- Simple, well-structured monad stack for reliable behavior

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

Man pages are automatically installed when using package managers like Homebrew:
```bash
# On macOS
brew tap fuzz/tap
brew install clod
# Or in one command:
brew install fuzz/tap/clod
```

When installing from Hackage, the man pages are included in the package.

When you install clod with `cabal install`, both the main `clod` program and the `cld` wrapper are installed automatically. The wrapper automatically opens the staging directory in your file browser after running clod.

### Prerequisites

- GHC (Glasgow Haskell Compiler) 9.0 or newer
- libgit2 (required for Git operations)
  - On macOS: `brew install libgit2`
  - On Linux: `apt-get install libgit2-dev` or equivalent for your distribution
  - On Windows: Install from source or use package manager

**Cross-Platform Support:** Clod works on macOS, Linux, and Windows. A wrapper script (`clod-open`) is included for opening the staging directory automatically in the appropriate file browser for each platform. Additionally, the `--print-path` option makes it easy to use clod with any command that accepts a directory path.

Supported file browsers:
* macOS: `open`
* Linux: `xdg-open`, `gio`, `gnome-open`, or `kde-open`
* Windows: `explorer.exe`

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

# Use the wrapper to process files and automatically open the staging directory
cld
```

### Command-Line Options

- `--all`, `-a`: Process all files, not just modified ones
- `--modified`, `-m`: Process only modified files (default)
- `--test`, `-t`: Run in test mode (no prompts, useful for CI)
- `--staging-dir DIR`, `-d DIR`: Specify a directory for test mode (only used with --test)
- `--verbose`, `-v`: Enable verbose output
- `--help`: Show help information
- `--version`: Show version information

### Wrapper Executable: `cld`

Clod comes with a wrapper executable that runs clod and automatically opens the staging directory:

```bash
# Process files and open the directory in your system's file browser
cld [all regular clod options]

# Process files without opening the directory
cld --no-open [all regular clod options]
```

The wrapper automatically detects your platform and uses the appropriate command to open the staging directory:
- macOS: `open`
- Linux: `xdg-open`, `gio`, `gnome-open`, or `kde-open`
- Windows: `explorer.exe`

When you install clod with `cabal install`, both the main program and the wrapper executable are installed automatically.

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

## Architecture

Clod uses a clean, pragmatic architecture with a focus on reliability and maintainability:

- **Traditional Monad Stack**: Uses a ReaderT/ExceptT/IO monad stack for excellent type inference and readable error messages
- **Capability-Based Security**: Runtime checking of file access permissions based on explicitly allowed directories
- **Modular Design**: Clear separation of concerns between different subsystems
- **Safety First**: Designed to prevent accidental access to unauthorized files

The architecture prioritizes pragmatic results over theoretical purity, focusing on delivering a reliable, easy-to-understand system that "just works."

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