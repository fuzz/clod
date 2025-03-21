#!/bin/bash
# Claude Git Project File Uploader
# Tracks modified files in a git repo and prepares them for upload to Claude's Project Knowledge
# Respects .gitignore patterns, skips package-lock.json and binary files

set -e  # Exit on errors

# Change to the git repository root
cd "$(git rev-parse --show-toplevel)" || { echo "Error: Not in a git repository"; exit 1; }
PROJECT_PATH="$(pwd)"

# Config files - store in the git repo under .claude-uploader
CONFIG_DIR="$PROJECT_PATH/.claude-uploader"
LAST_RUN_FILE="$CONFIG_DIR/last-run-marker"
STAGING_DIR="$HOME/Claude"

# Create config directory if it doesn't exist
mkdir -p "$CONFIG_DIR"
mkdir -p "$STAGING_DIR"

# Create timestamp directory for this run
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
CURRENT_STAGING="$STAGING_DIR/ClaudeUpload_$TIMESTAMP"
mkdir -p "$CURRENT_STAGING"

echo "Looking for modified files in $PROJECT_PATH..."

# Function to check if a file is text
is_text_file() {
    local file="$1"
    
    # Use the file command to check if it's a text file
    if file -b --mime-type "$file" | grep -q "^text/"; then
        return 0  # It's a text file
    elif [[ "$file" == *.md || "$file" == *.txt || "$file" == *.js || "$file" == *.jsx || "$file" == *.ts || "$file" == *.tsx || "$file" == *.html || "$file" == *.css || "$file" == *.scss || "$file" == *.json || "$file" == *.yaml || "$file" == *.yml || "$file" == *.xml || "$file" == *.svg || "$file" == *.sh || "$file" == *.py || "$file" == *.rb || "$file" == *.php ]]; then
        # Common text file extensions - sometimes file command can be incorrect
        return 0
    else
        return 1  # Not a text file
    fi
}

# Function to process a file
process_file() {
    local file="$1"
    local rel_path="${file#$PROJECT_PATH/}"
    
    # Skip specifically excluded files
    if [[ "$rel_path" == ".gitignore" || "$rel_path" == "package-lock.json" || "$rel_path" == "yarn.lock" ]]; then
        echo "Skipping: $rel_path (excluded file)"
        return
    fi
    
    # Skip binary files
    if ! is_text_file "$file"; then
        echo "Skipping: $rel_path (binary file)"
        return
    fi
    
    local dir_part=$(dirname "$rel_path")
    local file_name=$(basename "$rel_path")
    
    # Create optimized filename
    if [ "$dir_part" != "." ]; then
        # Replace slashes with dashes
        local optimized_name=$(echo "$dir_part" | tr '/' '-')
        optimized_name="$optimized_name-$file_name"
    else
        local optimized_name="$file_name"
    fi
    
    # Copy file with optimized name
    cp "$file" "$CURRENT_STAGING/$optimized_name"
    echo "Copied: $rel_path → $optimized_name"
}

file_count=0
skipped_count=0

# If last run file exists, find files newer than it
if [ -f "$LAST_RUN_FILE" ]; then
    echo "Finding files modified since last run..."
    
    # Get modification time of last run marker
    LAST_RUN_TIME=$(stat -f "%m" "$LAST_RUN_FILE")
    
    # Get all files tracked by git or untracked but not ignored
    while IFS= read -r file; do
        # Get full path
        full_path="$PROJECT_PATH/$file"
        
        # Skip if not a regular file
        if [ ! -f "$full_path" ]; then
            continue
        fi
        
        # Check if file is newer than last run
        file_time=$(stat -f "%m" "$full_path")
        if [ "$file_time" -gt "$LAST_RUN_TIME" ]; then
            # Try to process the file, track skipped files
            if process_file "$full_path"; then
                ((file_count++))
            else
                ((skipped_count++))
            fi
        fi
    done < <(git ls-files && git ls-files --others --exclude-standard)
else
    echo "First run - no previous timestamp found."
    echo "Options:"
    echo "  a: Import all files (respecting .gitignore)"
    echo "  m: Import only modified files (git status)"
    echo "  n: Import nothing (just set timestamp)"
    read -p "Choose an option [a/m/n]: " import_option
    
    case "$import_option" in
        a|A)
            echo "Importing all files (respecting .gitignore)..."
            
            # Get all files tracked by git or untracked but not ignored
            while IFS= read -r file; do
                # Get full path
                full_path="$PROJECT_PATH/$file"
                
                # Skip if not a regular file
                if [ ! -f "$full_path" ]; then
                    continue
                fi
                
                # Try to process the file, track skipped files
                if process_file "$full_path"; then
                    ((file_count++))
                else
                    ((skipped_count++))
                fi
            done < <(git ls-files && git ls-files --others --exclude-standard)
            ;;
            
        m|M)
            echo "Importing modified files from git status..."
            
            # Get modified files from git status
            while IFS= read -r file; do
                # Get full path
                full_path="$PROJECT_PATH/$file"
                
                # Skip if not a regular file
                if [ ! -f "$full_path" ]; then
                    continue
                fi
                
                # Try to process the file, track skipped files
                if process_file "$full_path"; then
                    ((file_count++))
                else
                    ((skipped_count++))
                fi
            done < <(git ls-files --modified && git ls-files --others --exclude-standard)
            ;;
            
        *)
            echo "Setting timestamp only."
            ;;
    esac
fi

# Update the last run marker
touch "$LAST_RUN_FILE"

if [ "$file_count" -eq 0 ]; then
    echo "No files processed (skipped: $skipped_count)."
    # Clean up the empty staging directory
    rmdir "$CURRENT_STAGING"
    exit 0
fi

# Open the staging directory
open "$CURRENT_STAGING"

echo "Success! $file_count files prepared for upload. Skipped: $skipped_count"
echo "Staging directory: $CURRENT_STAGING"
echo ""
echo "Next steps:"
echo "1. Navigate to Project Knowledge in your Claude project"
echo "2. Drag files from the staging folder"
echo "3. Start a new conversation to see changes"
