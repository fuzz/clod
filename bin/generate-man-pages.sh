#!/bin/bash
# Generate man pages from markdown documentation and content sources
# This script updates man page content from project source files

set -e

# Ensure pandoc is available
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is required to generate man pages"
    echo "Please install pandoc: https://pandoc.org/installing.html"
    exit 1
fi

# Go to project root
cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

# Use argument if provided, otherwise use current directory
OUTPUT_DIR="${1:-.}"

# Make sure the source man directory exists
mkdir -p "$PROJECT_ROOT/man"

# Make sure man section directories exist in the output
mkdir -p "$OUTPUT_DIR/man1"
mkdir -p "$OUTPUT_DIR/man7"
mkdir -p "$OUTPUT_DIR/man8"

# Check for required content source files
if [ ! -f "$PROJECT_ROOT/project-instructions.md" ]; then
    echo "Error: project-instructions.md not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/guardrails.md" ]; then
    echo "Error: guardrails.md not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/HUMAN.md" ]; then
    echo "Error: HUMAN.md not found"
    exit 1
fi

# Get the current version from cabal file
VERSION=$(grep -m 1 "^version:" "$PROJECT_ROOT/clod.cabal" | awk '{print $2}')
if [ -z "$VERSION" ]; then
    VERSION="0.1.0"  # Default if not found
fi

# Generate clod(1) - Command reference if it doesn't exist or update version
if [ ! -f "$PROJECT_ROOT/man/clod.1.md" ]; then
  echo "Generating clod(1).md source file..."
  cat > "$PROJECT_ROOT/man/clod.1.md" << EOF
% CLOD(1) Clod $VERSION
% Fuzz Leonard
% March 2025

# NAME

clod - Claude Loader for preparing files for Claude AI's Project Knowledge

# SYNOPSIS

**clod** [*OPTIONS*]

# DESCRIPTION

Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge feature. 
It tracks file changes, respects .gitignore and .clodignore patterns, and optimizes filenames 
for Claude's UI.

# OPTIONS

**--all**, **-a**
: Process all files (respecting .gitignore and .clodignore)

**--test**, **-t**
: Run in test mode (no prompts, useful for CI)

**--staging-dir** *DIR*, **-d** *DIR*
: Specify a directory for test mode (only used with --test)

**--verbose**, **-v**
: Enable verbose output

**--flush**, **-f**
: Remove missing entries from the database

**--last**, **-l**
: Reuse the previous staging directory

**--help**
: Show help information

**--version**
: Show version information

# EXAMPLES

Run clod (first run processes all files, subsequent runs process only modified files):
    clod

Force processing of all files:
    clod --all

Run in test mode with an optional test directory:
    clod --test --staging-dir /path/to/test/dir
    
Reuse the previous staging directory:
    clod --last
    
Remove missing entries from the database:
    clod --flush

# ENVIRONMENT VARIABLES

**CLOD_DIR**
: Override the default .clod directory name

**CLODIGNORE**
: Override the default .clodignore filename

# FILES

**.clodignore**
: Pattern file similar to .gitignore for excluding files

**.clod/database.dhall**
: Database of file checksums and metadata

# SEE ALSO

**clod(7)** for information about project instructions and safeguards.
**clod(8)** for a complete workflow guide to using clod with Claude AI.
EOF
else
  # Update version in existing file
  sed -i.bak "s/% CLOD(1) Clod [0-9.]\+/% CLOD(1) Clod $VERSION/" "$PROJECT_ROOT/man/clod.1.md"
  rm -f "$PROJECT_ROOT/man/clod.1.md.bak"
fi

# Update clod(7) with current content from project-instructions.md and guardrails.md
echo "Updating clod(7).md source file..."
cat > "$PROJECT_ROOT/man/clod.7.md" << EOF
% CLOD(7) Clod $VERSION
% Fuzz Leonard
% March 2025

# NAME

clod - project instructions and safeguards for Claude AI integration

# DESCRIPTION

This man page contains guidance on how to structure project instructions for Claude AI
and implement safeguards when using clod with Claude AI's Project Knowledge feature.
# PROJECT INSTRUCTIONS
EOF

# Append project-instructions.md content
cat "$PROJECT_ROOT/project-instructions.md" >> "$PROJECT_ROOT/man/clod.7.md"

# Append guardrails.md content
echo "" >> "$PROJECT_ROOT/man/clod.7.md"
echo "# SAFEGUARDS" >> "$PROJECT_ROOT/man/clod.7.md"
cat "$PROJECT_ROOT/guardrails.md" >> "$PROJECT_ROOT/man/clod.7.md"

# Update clod(8) with current content from HUMAN.md
echo "Updating clod(8).md source file..."
cat > "$PROJECT_ROOT/man/clod.8.md" << EOF
% CLOD(8) Clod $VERSION
% Fuzz Leonard
% March 2025

# NAME

clod - complete workflow guide for using clod with Claude AI

# DESCRIPTION

This man page contains a comprehensive guide to using clod with Claude AI,
including best practices, workflow details, and integration tips.
EOF

# Append HUMAN.md content
cat "$PROJECT_ROOT/HUMAN.md" >> "$PROJECT_ROOT/man/clod.8.md"

# Generate man pages from markdown
pandoc -s -t man "$PROJECT_ROOT/man/clod.1.md" -o "$OUTPUT_DIR/man1/clod.1"
pandoc -s -t man "$PROJECT_ROOT/man/clod.7.md" -o "$OUTPUT_DIR/man7/clod.7"
pandoc -s -t man "$PROJECT_ROOT/man/clod.8.md" -o "$OUTPUT_DIR/man8/clod.8"

echo "Man page source files updated in $PROJECT_ROOT/man/"
echo "Generated man pages are in $OUTPUT_DIR/man{1,7,8}/"