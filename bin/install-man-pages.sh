#!/bin/bash
# Install man pages from pre-generated markdown files
# This script is meant to be used during package installation

# Enable debugging
set -e
set -x  # Print commands and their arguments as they are executed

# Ensure pandoc is available
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is required to install man pages"
    echo "Please install pandoc: https://pandoc.org/installing.html"
    exit 1
fi

# Go to project root
cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

# Debug info
echo "Current directory: $(pwd)"
echo "Script location: $0"
echo "Script directory: $(dirname "$0")"
ls -la "$PROJECT_ROOT/man" || echo "man directory not found"

# Use argument if provided, otherwise use current directory
OUTPUT_DIR="${1:-.}"

# Make sure man section directories exist
mkdir -p "$OUTPUT_DIR/man1"
mkdir -p "$OUTPUT_DIR/man7"
mkdir -p "$OUTPUT_DIR/man8"

# Check if the source man directory exists
if [ ! -d "$PROJECT_ROOT/man" ]; then
    echo "Warning: Man page source directory not found at $PROJECT_ROOT/man"
    echo "Will look for man pages in the project root directory"
else
    echo "Man page directory found at $PROJECT_ROOT/man"
fi

# Check for man page source files, but don't fail the build if missing
missing_files=0

# List all files in the directory for debugging
echo "Contents of $PROJECT_ROOT:"
find "$PROJECT_ROOT" -type f -name "*.md" | grep -i "man" || echo "No man pages found with find"

# Look for man pages in multiple possible locations (both in man/ and in the root)
MAN1_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.1.md" ]; then
    MAN1_PATH="$PROJECT_ROOT/man/clod.1.md"
elif [ -f "$PROJECT_ROOT/clod.1.md" ]; then
    MAN1_PATH="$PROJECT_ROOT/clod.1.md"
fi

if [ -z "$MAN1_PATH" ]; then
    echo "Warning: clod.1.md not found - man1 page will not be installed"
    missing_files=1
else
    echo "Found man1 page at: $MAN1_PATH"
fi

# Look for man7 page
MAN7_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.7.md" ]; then
    MAN7_PATH="$PROJECT_ROOT/man/clod.7.md"
elif [ -f "$PROJECT_ROOT/clod.7.md" ]; then
    MAN7_PATH="$PROJECT_ROOT/clod.7.md"
fi

if [ -z "$MAN7_PATH" ]; then
    echo "Warning: clod.7.md not found - man7 page will not be installed"
    missing_files=1
else
    echo "Found man7 page at: $MAN7_PATH"
fi

# Look for man8 page
MAN8_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.8.md" ]; then
    MAN8_PATH="$PROJECT_ROOT/man/clod.8.md"
elif [ -f "$PROJECT_ROOT/clod.8.md" ]; then
    MAN8_PATH="$PROJECT_ROOT/clod.8.md"
fi

if [ -z "$MAN8_PATH" ]; then
    echo "Warning: clod.8.md not found - man8 page will not be installed"
    missing_files=1
else
    echo "Found man8 page at: $MAN8_PATH"
fi

if [ "$missing_files" -eq 1 ]; then
    echo "Some man pages are missing but continuing with installation"
fi

# Generate only the man pages that exist
if [ -n "$MAN1_PATH" ]; then
    echo "Generating man1 page from $MAN1_PATH"
    pandoc -s -t man "$MAN1_PATH" -o "$OUTPUT_DIR/man1/clod.1"
    echo "Man page installed to $OUTPUT_DIR/man1/"
fi

if [ -n "$MAN7_PATH" ]; then
    echo "Generating man7 page from $MAN7_PATH"
    pandoc -s -t man "$MAN7_PATH" -o "$OUTPUT_DIR/man7/clod.7"
    echo "Man page installed to $OUTPUT_DIR/man7/"
fi

if [ -n "$MAN8_PATH" ]; then
    echo "Generating man8 page from $MAN8_PATH"
    pandoc -s -t man "$MAN8_PATH" -o "$OUTPUT_DIR/man8/clod.8"
    echo "Man page installed to $OUTPUT_DIR/man8/"
fi

echo "Man page installation completed"