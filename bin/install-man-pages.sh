#!/bin/bash
# Install man pages from pre-generated markdown files
# This script is meant to be used during package installation

set -e

# Ensure pandoc is available
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is required to install man pages"
    echo "Please install pandoc: https://pandoc.org/installing.html"
    exit 1
fi

# Go to project root
cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

# Use argument if provided, otherwise use current directory
OUTPUT_DIR="${1:-.}"

# Make sure man section directories exist
mkdir -p "$OUTPUT_DIR/man1"
mkdir -p "$OUTPUT_DIR/man7"
mkdir -p "$OUTPUT_DIR/man8"

# Make sure the source man directory exists
if [ ! -d "$PROJECT_ROOT/man" ]; then
    echo "Error: Man page source directory not found at $PROJECT_ROOT/man"
    exit 1
fi

# Check that all required man page source files exist
if [ ! -f "$PROJECT_ROOT/man/clod.1.md" ]; then
    echo "Error: clod.1.md not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/man/clod.7.md" ]; then
    echo "Error: clod.7.md not found"
    exit 1
fi

if [ ! -f "$PROJECT_ROOT/man/clod.8.md" ]; then
    echo "Error: clod.8.md not found"
    exit 1
fi

# Generate the man pages from the pre-existing markdown files
pandoc -s -t man "$PROJECT_ROOT/man/clod.1.md" -o "$OUTPUT_DIR/man1/clod.1"
pandoc -s -t man "$PROJECT_ROOT/man/clod.7.md" -o "$OUTPUT_DIR/man7/clod.7"
pandoc -s -t man "$PROJECT_ROOT/man/clod.8.md" -o "$OUTPUT_DIR/man8/clod.8"

echo "Man pages installed to $OUTPUT_DIR/man{1,7,8}/"