# Clod - Claude Loader

[![Hackage](https://img.shields.io/hackage/v/clod.svg)](https://hackage.haskell.org/package/clod)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Clod is a utility for preparing and uploading files to Claude AI's Project
Knowledge feature. It tracks file changes using checksums, respects
`.gitignore` and `.clodignore` patterns, optimizes filenames for Claude's UI
and provides a filename manifest so Claude can write the files back to their
original locations. By efficiently handling file selection and staging, it can
significantly reduce AI development costs by 50% or more. Unlike other tools
created to solve this problem `clod` does not require any unauthorized access
to Anthropic products, nor is it affected by changes to Claude's UI.
Contributions of Automator code to handle the drag and drops and Project
Knowledge deletes on macOS are welcome, as is similar code for other platforms.

```
# On macOS, process files and open the staging directory in Finder
open `clod`
```

Why use Clod instead of just MCP? We use MCP to write to the local filesystem,
but using MCP to read everything is less efficient than using Project Knowledge.
Anthropic only allows Project Knowledge files to be managed via the UI--there
is no API access, no MCP access, Claude cannot update them.

A typical workflow for me is to work with Claude App on a new feature for a few
iterations, until he gets stuck trying to get a test to pass or something. Then
I hand it over to Claude Code and let him solve the final problems with local
access and also have him double-check the other Claude’s work. I say “save 50%”
but really I probably save more like 90% with this approach.

Claude App is more conversational than Claude Code and he can render web pages
and SVGs and so on. Claude Code will often hit a minor roadblock when
implementing something and turn around and do exactly what you told him not to
do--this is rarely an issue with Claude App as there's more human in the loop.
Coding with Claude App isn't just about cost savings for me--I genuinely prefer
it for many use cases.

Added April 16, 2025: Claude App has a new 'select all' feature in the Project
Knowledge section that now makes the easiest workflow to just delete all Project
Knowledge files and replace them with the contents of `clod --all`. Note this
approach could have an impact on your usage limits, although as of today I have
not observed this to be the case.

Developed by [Fuzz, Inc](https://fuzz.ink) - World-class technical leadership and execution

## Features

- Track modified files using checksums for accuracy
- Detect renamed files by matching content checksums
- Respect `.gitignore` and `.clodignore` patterns
- Handle binary vs. text files automatically
- Use system temporary directories for staging files
- Create optimized filenames for Claude's UI
- Generate a path manifest for mapping optimized names back to original paths
- Capability-based security for file operations

### First Run

On first run, Clod will:

1. Create a system temporary directory for staging files
2. Create a default `.clodignore` file if one doesn't exist
3. Prompt you to choose which files to process:
   - All files
   - Only modified files
   - None (just set timestamp)

### Integration with Claude AI

First time: Paste the contents of `project-instructions.md` into the Project
Instructions section
 
After running Clod:

1. Navigate to Project Knowledge in your Claude Project (Pro or Team account
   required)
2. Drag files from the opened staging folder to Project Knowledge
3. Include the `_path_manifest.dhall` file which maps optimized names back to
   original paths
4. **Important**: You must manually delete previous versions of these files
   from Project Knowledge before starting a new conversation to ensure Claude
   uses the most recent files
5. Note that the staging directory is temporary and will be cleaned up on your
   next run of clod (or system reboot)

## Installation

## Homebrew (binary is Apple Silicon only for now)

```bash
# On macOS
brew tap fuzz/tap
brew install clod
```

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

### Prerequisites

- Claude Pro, Max, Teams or Enteprise account
- Claude desktop app for filesystem access (currently only macOS and Windows)
- GHC (Glasgow Haskell Compiler) 9.0 or newer
- libmagic (required for file type detection)

**Cross-Platform Support:** Clod has been tested on macOS, but should work on
Linux and Windows. The program outputs the path to the staging directory,
making it easy to open with your system's file browser or use with any command
that accepts a directory path.

* macOS: `open`
* Linux: `xdg-open`, `gio`, `gnome-open`, or `kde-open`
* Windows: `explorer.exe`

Pull requests for improved cross-platform support are welcome.

## Usage

### Basic Usage

```bash
# Process all files (first run) or modified files since last run
clod

# Process all files regardless of last run (respecting .gitignore and .clodignore)
clod --all

# On macOS, process files and open the staging directory in Finder
open `clod`
```



### Command-Line Options

- `--all`, `-a`: Process all files, not just modified ones
- `--test`, `-t`: Run in test mode (no prompts, useful for CI)
- `--staging-dir DIR`, `-d DIR`: Specify a directory for test mode (only used with --test)
- `--verbose`, `-v`: Enable verbose output
- `--flush`, `-f`: Flush stale entries from the checksums database
- `--last`, `-l`: Reuse the previous staging directory
- `--help`: Show help information
- `--version`, `-V`: Show version information

### Opening the Staging Directory

Clod outputs the path to the staging directory, which you can use to open it
directly in your file browser:

```bash
# On macOS, process files and open the directory in Finder
open `clod`

# For scripts, you can capture the output and open it with your preferred application
STAGING_DIR=$(clod [options])

# Open with the appropriate command for your platform
# macOS
open "$STAGING_DIR"
# Linux
xdg-open "$STAGING_DIR"  # or gio, gnome-open, kde-open
# Windows
explorer.exe "$STAGING_DIR"
```

## Configuration

### Environment Variables

You can customize Clod's behavior using these environment variables:

- `CLOD_DIR` - Override the default `.clod` directory name
- `CLODIGNORE` - Override the default `.clodignore` filename

### .clodignore

A `.clodignore` file in your repository root specifies files or patterns to
exclude. If this file doesn't exist, Clod will create a default one for you
with common patterns for binary files, build directories, and large files.

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
