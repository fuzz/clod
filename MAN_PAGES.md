# Man Page Integration for Clod

This document explains how the man page generation and installation system works for Clod.

## Overview

Clod provides comprehensive man pages following standard Unix conventions:

- `clod(1)` - Command usage and options
- `clod(7)` - Project instructions and safeguards
- `clod(8)` - Workflow guide

The man pages are generated from Markdown sources in the `man/` directory.

## Source Files

The man page source files are Markdown files:

- `man/clod.1.md` - Command reference (section 1)
- `man/clod.7.md` - Project instructions (section 7)
- `man/clod.8.md` - Workflow guide (section 8)

## Generation Process

Man pages are generated during Homebrew installation using the `bin/generate-man-pages.sh` script:

1. The script takes the source .md files and converts them to man format using pandoc
2. Generated man pages are installed to the proper Homebrew man directories
3. This makes them available system-wide via the standard `man clod` command

## Testing and Development

When working with man pages:

1. **Editing Content**: Update the Markdown files in the `man/` directory
2. **Testing Generation**: Run `bin/generate-man-pages.sh` to generate man pages in the current directory
3. **Viewing Locally**: Use `man -l man1/clod.1` to view the generated man pages

## Homebrew Integration

The Homebrew formula in `homebrew-tap/Formula/clod.rb` handles man page installation:

1. During the install process, it runs the generate-man-pages.sh script
2. The generated man pages are copied to the proper system locations
3. Users can then access the documentation with standard man commands