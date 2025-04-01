#!/bin/bash
# Generate man pages from markdown documentation
# This script regenerates man pages from scratch using source files

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

# Make sure the source man directory exists
mkdir -p "$PROJECT_ROOT/man"

# Get the current version from cabal file
VERSION=$(grep -m 1 "^version:" "$PROJECT_ROOT/clod.cabal" | awk '{print $2}')
if [ -z "$VERSION" ]; then
    VERSION="0.1.0"  # Default if not found
fi

# Generate clod(1) - Command reference
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

# Generate clod(7) - Project instructions and safeguards
if [ -f "$PROJECT_ROOT/project-instructions.md" ] && [ -f "$PROJECT_ROOT/guardrails.md" ]; then
  echo "Generating clod(7).md source file..."
  
  # Create the header section
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
else
  echo "Warning: Cannot generate clod(7).md, source files missing"
fi

# Generate clod(8) - Complete workflow guide
if [ -f "$PROJECT_ROOT/HUMAN.md" ]; then
  echo "Generating clod(8).md source file..."
  
  # Create the header section
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
else
  echo "Warning: Cannot generate clod(8).md, HUMAN.md missing"
fi

echo "Man page markdown generation completed"