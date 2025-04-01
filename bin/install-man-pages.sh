#!/bin/bash
# Install man pages from pre-generated markdown files
# This script is meant to be used during package installation

# Create a dedicated log file
LOG_FILE="/tmp/clod-install-man-pages-$(date +%s).log"
touch "$LOG_FILE"
echo "Logging to $LOG_FILE"

# Log functions 
log() {
    echo "$(date): $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo "ERROR: $1" | tee -a "$LOG_FILE" >&2
}

# Start logging
log "Starting man page installation"
log "Running as user: $(whoami)"
log "Current path: $PATH"
log "Current working directory: $(pwd)"

# Don't exit on errors, but log them
set +e

# Try multiple ways to find pandoc
PANDOC_CMD=""
if command -v pandoc &>/dev/null; then
    PANDOC_CMD="pandoc"
    log "Found pandoc in PATH"
elif [ -x "/opt/homebrew/bin/pandoc" ]; then
    PANDOC_CMD="/opt/homebrew/bin/pandoc"
    log "Using Homebrew pandoc"
elif [ -x "/usr/local/bin/pandoc" ]; then
    PANDOC_CMD="/usr/local/bin/pandoc"
    log "Using /usr/local/bin/pandoc"
else
    log_error "Pandoc not found. Man pages will not be generated."
    # Create empty files so installation can continue
    if [ ! -z "$1" ]; then
        mkdir -p "$1/man1" "$1/man7" "$1/man8"
        touch "$1/man1/clod.1" "$1/man7/clod.7" "$1/man8/clod.8"
        log "Created empty man page files in $1"
    fi
    exit 0
fi

# Go to project root
cd "$(dirname "$0")/.." || {
    log_error "Failed to change to project root directory"
    exit 0
}
PROJECT_ROOT=$(pwd)

# Debug info
log "Current directory: $(pwd)"
log "Script location: $0"
log "Script directory: $(dirname "$0")"

# Check for man directory
if [ -d "$PROJECT_ROOT/man" ]; then
    log "Man directory found at $PROJECT_ROOT/man"
    ls -la "$PROJECT_ROOT/man" >> "$LOG_FILE" 2>&1 || log_error "Failed to list man directory content"
else 
    log_error "Man directory not found at $PROJECT_ROOT/man"
    # Create empty man files and exit
    if [ ! -z "$1" ]; then
        mkdir -p "$1/man1" "$1/man7" "$1/man8"
        touch "$1/man1/clod.1" "$1/man7/clod.7" "$1/man8/clod.8"
        log "Created empty man page files in $1"
    fi
    exit 0
fi

# Use argument if provided, otherwise use current directory
OUTPUT_DIR="${1:-.}"
log "Output directory set to $OUTPUT_DIR"

# Make sure man section directories exist
mkdir -p "$OUTPUT_DIR/man1" || log_error "Failed to create man1 directory"
mkdir -p "$OUTPUT_DIR/man7" || log_error "Failed to create man7 directory"
mkdir -p "$OUTPUT_DIR/man8" || log_error "Failed to create man8 directory"

log "Created man directories in $OUTPUT_DIR"

# Check for man page source files, but don't fail the build if missing
missing_files=0

# List all man pages for debugging
log "Looking for man pages in $PROJECT_ROOT"
find "$PROJECT_ROOT" -type f -name "*.md" | grep -i "man" >> "$LOG_FILE" 2>&1 || log "No man pages found with find"

# Look for man pages in multiple possible locations
MAN1_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.1.md" ]; then
    MAN1_PATH="$PROJECT_ROOT/man/clod.1.md"
    log "Found man1 page at: $MAN1_PATH"
elif [ -f "$PROJECT_ROOT/clod.1.md" ]; then
    MAN1_PATH="$PROJECT_ROOT/clod.1.md"
    log "Found man1 page at: $MAN1_PATH"
fi

if [ -z "$MAN1_PATH" ]; then
    log_error "clod.1.md not found - man1 page will not be installed"
    # Create empty file
    touch "$OUTPUT_DIR/man1/clod.1"
    missing_files=1
fi

# Look for man7 page
MAN7_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.7.md" ]; then
    MAN7_PATH="$PROJECT_ROOT/man/clod.7.md"
    log "Found man7 page at: $MAN7_PATH"
elif [ -f "$PROJECT_ROOT/clod.7.md" ]; then
    MAN7_PATH="$PROJECT_ROOT/clod.7.md"
    log "Found man7 page at: $MAN7_PATH"
fi

if [ -z "$MAN7_PATH" ]; then
    log_error "clod.7.md not found - man7 page will not be installed"
    # Create empty file
    touch "$OUTPUT_DIR/man7/clod.7"
    missing_files=1
fi

# Look for man8 page
MAN8_PATH=""
if [ -f "$PROJECT_ROOT/man/clod.8.md" ]; then
    MAN8_PATH="$PROJECT_ROOT/man/clod.8.md"
    log "Found man8 page at: $MAN8_PATH"
elif [ -f "$PROJECT_ROOT/clod.8.md" ]; then
    MAN8_PATH="$PROJECT_ROOT/clod.8.md"
    log "Found man8 page at: $MAN8_PATH"
fi

if [ -z "$MAN8_PATH" ]; then
    log_error "clod.8.md not found - man8 page will not be installed"
    # Create empty file
    touch "$OUTPUT_DIR/man8/clod.8"
    missing_files=1
fi

if [ "$missing_files" -eq 1 ]; then
    log "Some man pages are missing but continuing with installation"
fi

# Generate only the man pages that exist
if [ -n "$MAN1_PATH" ]; then
    log "Generating man1 page from $MAN1_PATH"
    "$PANDOC_CMD" -s -t man "$MAN1_PATH" -o "$OUTPUT_DIR/man1/clod.1" 2>> "$LOG_FILE" || {
        log_error "Failed to generate man1 page. Creating empty file."
        touch "$OUTPUT_DIR/man1/clod.1"
    }
    log "Man page installed to $OUTPUT_DIR/man1/"
fi

if [ -n "$MAN7_PATH" ]; then
    log "Generating man7 page from $MAN7_PATH"
    "$PANDOC_CMD" -s -t man "$MAN7_PATH" -o "$OUTPUT_DIR/man7/clod.7" 2>> "$LOG_FILE" || {
        log_error "Failed to generate man7 page. Creating empty file."
        touch "$OUTPUT_DIR/man7/clod.7"
    }
    log "Man page installed to $OUTPUT_DIR/man7/"
fi

if [ -n "$MAN8_PATH" ]; then
    log "Generating man8 page from $MAN8_PATH"
    "$PANDOC_CMD" -s -t man "$MAN8_PATH" -o "$OUTPUT_DIR/man8/clod.8" 2>> "$LOG_FILE" || {
        log_error "Failed to generate man8 page. Creating empty file."
        touch "$OUTPUT_DIR/man8/clod.8"
    }
    log "Man page installed to $OUTPUT_DIR/man8/"
fi

log "Man page installation completed"
exit 0