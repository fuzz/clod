#!/bin/bash
# Claude Git Project File Uploader
# Tracks modified files in a git repo and prepares them for upload to Claude's Project Knowledge
# Respects .gitignore patterns, skips package-lock.json and binary files

set -e  # Exit on errors

# Version check
VERSION="1.0.0"
echo "clod version $VERSION"

# Platform detection
PLATFORM="$(uname -s)"
if [[ "$PLATFORM" != "Darwin" ]]; then
    echo "Warning: clod is primarily designed for macOS. Some features may not work on $PLATFORM."
    echo "Claude's filesystem access is currently only available on macOS and Windows desktop applications."
fi

# Check for required dependencies
if ! command -v git &> /dev/null; then
    echo "Error: git is required but not installed or not in PATH"
    exit 1
fi

# Change to the git repository root
cd "$(git rev-parse --show-toplevel)" || { echo "Error: Not in a git repository"; exit 1; }
PROJECT_PATH="$(pwd)"

# Check for uncommitted changes
if [[ -n "$(git status --porcelain)" ]]; then
    echo "Warning: You have uncommitted changes in your repository."
    echo "It's recommended to commit your changes before running clod to ensure you can recover if needed."
    
    # In test mode, automatically proceed with "Y"
    if [[ -n "${CLOD_TEST_MODE}" ]]; then
        echo "Test mode: automatically continuing..."
        REPLY="Y"
    else
        read -p "Continue anyway? [y/N] " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit 1
        fi
    fi
fi

# Allow user to configure staging directory
DEFAULT_STAGING_DIR="$HOME/Claude"

# In test mode, use the provided staging directory or a default test location
if [[ -n "${CLOD_TEST_MODE}" ]]; then
    STAGING_DIR="${CLOD_TEST_STAGING_DIR:-$DEFAULT_STAGING_DIR}"
    echo "Test mode: using staging directory $STAGING_DIR"
else
    read -p "Staging directory [$DEFAULT_STAGING_DIR]: " STAGING_DIR
    STAGING_DIR=${STAGING_DIR:-$DEFAULT_STAGING_DIR}
fi

# Config files - store in the git repo under .claude-uploader
CONFIG_DIR="$PROJECT_PATH/.claude-uploader"
LAST_RUN_FILE="$CONFIG_DIR/last-run-marker"

# Create config directory if it doesn't exist
mkdir -p "$CONFIG_DIR"
mkdir -p "$STAGING_DIR"

# Create timestamp directory for this run
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
CURRENT_STAGING="$STAGING_DIR/ClaudeUpload_$TIMESTAMP"
mkdir -p "$CURRENT_STAGING"

echo "Looking for modified files in $PROJECT_PATH..."

# Initialize path manifest
echo "{" > "$CURRENT_STAGING/_path_manifest.json"
FIRST_ENTRY=true

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
        return 1
    fi
    
    # Skip binary files
    if ! is_text_file "$file"; then
        echo "Skipping: $rel_path (binary file)"
        return 1
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
    
    # Add to path manifest
    if [ "$FIRST_ENTRY" = true ]; then
        FIRST_ENTRY=false
    else
        echo "," >> "$CURRENT_STAGING/_path_manifest.json"
    fi
    
    # Escape JSON special characters
    local escaped_optimized_name=$(echo "$optimized_name" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')
    local escaped_rel_path=$(echo "$rel_path" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')
    
    echo "  \"$escaped_optimized_name\": \"$escaped_rel_path\"" >> "$CURRENT_STAGING/_path_manifest.json"
    
    echo "Copied: $rel_path → $optimized_name"
    return 0
}

file_count=0
skipped_count=0

# If last run file exists, find files newer than it
if [ -f "$LAST_RUN_FILE" ]; then
    echo "Finding files modified since last run..."
    
    # Get modification time of last run marker
    LAST_RUN_TIME=$(stat -f "%m" "$LAST_RUN_FILE")
    
    # Get all modified files according to git, ensuring each file is only processed once
    while IFS= read -r file; do
        # Skip empty lines
        [ -z "$file" ] && continue
        
        # Get full path
        full_path="$PROJECT_PATH/$file"
        
        # Skip if not a regular file
        if [ ! -f "$full_path" ]; then
            continue
        fi
        
        # Skip any files in the staging directory
        if [[ "$full_path" == *"$STAGING_DIR"* ]]; then
            echo "Skipping: $full_path (in staging directory)"
            continue
        fi
        
        # Try to process the file, track skipped files
        if process_file "$full_path"; then
            ((file_count++))
        else
            ((skipped_count++))
        fi
    done < <({ git ls-files --modified && git diff --name-only && git ls-files --others --exclude-standard; } | sort -u)
else
    echo "First run - no previous timestamp found."
    
    # In test mode, automatically choose option 'a'
    if [[ -n "${CLOD_TEST_MODE}" ]]; then
        import_option="a"
        echo "Test mode: automatically importing all files"
    else
        echo "Options:"
        echo "  a: Import all files (respecting .gitignore)"
        echo "  m: Import only modified files (git status)"
        echo "  n: Import nothing (just set timestamp)"
        read -p "Choose an option [a/m/n]: " import_option
    fi
    
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
                
                # Skip any files in the staging directory
                if [[ "$full_path" == *"$STAGING_DIR"* ]]; then
                    echo "Skipping: $full_path (in staging directory)"
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

# Close the path manifest JSON
echo "}" >> "$CURRENT_STAGING/_path_manifest.json"

# Create README.txt with instructions
cat > "$CURRENT_STAGING/README.txt" << EOF
# Claude Upload Directory

This directory contains files prepared for upload to Claude's Project Knowledge:

1. Your code files with optimized names where directories are converted to prefixes with dashes
   - Example: components/Header.jsx → components-Header.jsx

2. A _path_manifest.json file mapping optimized names back to original paths
   - This is used by Claude to write changes back to the correct locations

## Next Steps:

1. Navigate to Claude's Project Knowledge section
2. Drag and drop all these files
3. Upload the project-instructions.md file from the clod repository
4. Start coding with Claude!
EOF

# Update the last run marker
touch "$LAST_RUN_FILE"

if [ "$file_count" -eq 0 ]; then
    echo "No files processed (skipped: $skipped_count)."
    # Clean up the empty staging directory
    rm -f "$CURRENT_STAGING/_path_manifest.json"
    rm -f "$CURRENT_STAGING/README.txt"
    rmdir "$CURRENT_STAGING"
    exit 0
fi

# Open the staging directory (skip in test mode)
if [[ -z "${CLOD_TEST_MODE}" ]]; then
    open "$CURRENT_STAGING"
fi

echo "Success! $file_count files prepared for upload. Skipped: $skipped_count"
echo "Staging directory: $CURRENT_STAGING"

# Only show next steps if not in test mode
if [[ -z "${CLOD_TEST_MODE}" ]]; then
    echo ""
    echo "Next steps:"
    echo "1. Navigate to Project Knowledge in your Claude project"
    echo "2. Drag files from the staging folder"
    echo "3. Start a new conversation to see changes"
fi
