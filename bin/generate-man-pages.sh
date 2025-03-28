#!/bin/bash
# Generate man pages from markdown documentation

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

# Create man directory if it doesn't exist
mkdir -p "$PROJECT_ROOT/man"

# Generate clod(1) - Command reference
echo "Generating clod(1) man page..."
cat > "$PROJECT_ROOT/man/clod.1.md" << EOF
% CLOD(1) Clod 0.1.0
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

**--modified**, **-m**
: Process only modified files (default)

**--test**, **-t**
: Run in test mode (no prompts, useful for CI)

**--staging-dir** *DIR*, **-d** *DIR*
: Specify a directory for test mode (only used with --test)

**--verbose**, **-v**
: Enable verbose output

**--help**
: Show help information

**--version**
: Show version information

# EXAMPLES

Process modified files since last run:
    clod

Process all files:
    clod --all

Run in test mode with an optional test directory:
    clod --test --staging-dir /path/to/test/dir

# ENVIRONMENT VARIABLES

**CLOD_DIR**
: Override the default .clod directory name

**CLODIGNORE**
: Override the default .clodignore filename

# FILES

**.clodignore**
: Pattern file similar to .gitignore for excluding files

**.clod/last-run-marker**
: File that marks when clod was last run

# SEE ALSO

**clod(7)** for information about project instructions and safeguards.
**clod(8)** for a complete workflow guide to using clod with Claude AI.
EOF

# Extract content from README for clod(1)
pandoc -s -t man "$PROJECT_ROOT/man/clod.1.md" -o "$PROJECT_ROOT/man/clod.1"

# Generate clod(7) - Project instructions and safeguards
echo "Generating clod(7) man page..."
cat > "$PROJECT_ROOT/man/clod.7.md" << EOF
% CLOD(7) Clod 0.1.0
% Fuzz Leonard
% March 2025

# NAME

clod - project instructions and safeguards for Claude AI integration

# DESCRIPTION

This man page contains guidance on how to structure project instructions for Claude AI
and implement safeguards when using clod with Claude AI's Project Knowledge feature.
EOF

# Append project-instructions.md and guardrails.md content
{
  echo "# PROJECT INSTRUCTIONS"
  cat "$PROJECT_ROOT/project-instructions.md"
  echo ""
  echo "# SAFEGUARDS"
  cat "$PROJECT_ROOT/guardrails.md"
} >> "$PROJECT_ROOT/man/clod.7.md"

# Generate man page from combined markdown
pandoc -s -t man "$PROJECT_ROOT/man/clod.7.md" -o "$PROJECT_ROOT/man/clod.7"

# Generate clod(8) - Complete workflow guide
echo "Generating clod(8) man page..."
cat > "$PROJECT_ROOT/man/clod.8.md" << EOF
% CLOD(8) Clod 0.1.0
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

# Generate man page
pandoc -s -t man "$PROJECT_ROOT/man/clod.8.md" -o "$PROJECT_ROOT/man/clod.8"

echo "Man pages generated successfully in $PROJECT_ROOT/man/"