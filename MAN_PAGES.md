# Man Page Integration for Clod

This document explains how the man page generation and installation system works for Clod.

## Overview

Clod provides comprehensive man pages following standard Unix conventions:

- `clod(1)` - Command usage and options
- `clod(7)` - Project instructions and safeguards
- `clod(8)` - Workflow guide

The man pages are generated from Markdown sources in the `man/` directory and are automatically installed when building and installing the package.

## How Man Page Generation Works

### Source Files

The man page source files are Markdown files:

- `man/clod.1.md` - Command reference (section 1)
- `man/clod.7.md` - Project instructions (section 7)
- `man/clod.8.md` - Workflow guide (section 8)

### Generation Process

1. The man pages are generated as part of the Cabal build process.
2. The generation is handled by a custom `Setup.hs` script.
3. The script checks if pandoc is available and uses it to convert the Markdown files to man format.
4. The `bin/generate-man-pages.sh` script is called by the Setup.hs script.
5. Generated man pages are placed in the build directory.
6. During installation, the man pages are installed to the appropriate location.

## Implementation Details

### Cabal Integration

Man page generation and installation is integrated with Cabal through:

1. **Custom Setup.hs**: Provides hooks for post-build and copy operations.

2. **Data Files Declaration**: In `clod.cabal`, the man pages are declared as data files:
   ```
   data-files:
     man/man1/clod.1,
     man/man7/clod.7,
     man/man8/clod.8
   ```

3. **Source Files**: The Markdown sources are included as extra-source-files.

### Setup.hs Details

The `Setup.hs` script uses Cabal's hook system to:

1. Generate man pages during the post-build phase.
2. Install them to the appropriate directories during the copy phase.

Key functions:
- `generateManPages`: Calls the shell script to convert Markdown to man format.
- `installManPages`: Copies generated man pages to the installation directories.
- `doesPandocExist`: Checks if pandoc is available on the system.

### Homebrew Integration

The Homebrew formula in `homebrew-tap/Formula/clod.rb` is designed to:

1. Install the package with Cabal.
2. Copy the man pages from the Cabal installation to Homebrew's man directories.
3. Fall back to generating them directly if they aren't available in the package.

## Testing

A dedicated test module `Clod.ManPagesSpec` verifies that:

1. Man page source files exist.
2. The generation script exists and is executable.
3. Dependencies like pandoc are available (when possible).

## For Developers

When working with man pages:

1. **Editing Content**: Update the Markdown files in the `man/` directory.
2. **Testing Generation**: Run `bin/generate-man-pages.sh` to verify your changes.
3. **Viewing Generated Pages**: Use `man -l man1/clod.1` to view locally.
4. **Structure**: Follow the standard man page conventions for sections.

## Important Notes

- Man page generation requires pandoc.
- If pandoc is not available during build, man pages won't be generated.
- The Homebrew formula has a fallback mechanism to generate man pages if they aren't included in the package.
- The `Setup.hs` script is designed to gracefully handle missing dependencies.