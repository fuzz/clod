#!/bin/bash
# Test script for clod functionality
# This script verifies that clod is working correctly by running various test cases

set -e  # Exit on errors

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Save the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo -e "${YELLOW}Running tests for clod...${NC}"

# Create a temporary test directory
TEST_DIR=$(mktemp -d)
cd "$TEST_DIR"

echo "Working in test directory: $TEST_DIR"

# Initialize a git repository
echo -e "\n${YELLOW}Test: Setting up git repository...${NC}"
git init > /dev/null
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Git repository initialized${NC}"
else
    echo -e "${RED}✗ Failed to initialize git repository${NC}"
    exit 1
fi

# Test file creation
echo -e "\n${YELLOW}Test: Creating test files...${NC}"

# Create directories
mkdir -p src/components
mkdir -p src/utils
mkdir -p test
mkdir -p public

# Create text files
echo "# Test Project" > README.md
echo "export const add = (a, b) => a + b;" > src/utils/math.js
echo "<div>Header Component</div>" > src/components/Header.jsx
echo "console.log('Hello, World!');" > src/index.js
echo "test('adds 1 + 2 to equal 3', () => { expect(add(1, 2)).toBe(3); });" > test/math.test.js

# Create an SVG file for testing SVG handling
echo '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100"><circle cx="50" cy="50" r="40" fill="blue" /></svg>' > public/logo.svg

# Create a binary file
dd if=/dev/urandom of=binary-file.bin bs=1024 count=1 2> /dev/null

# Add files to git
git config --local user.email "test@example.com"
git config --local user.name "Test User"
git add . > /dev/null
git commit -m "Initial commit" > /dev/null

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Test files created and committed${NC}"
else
    echo -e "${RED}✗ Failed to create test files${NC}"
    exit 1
fi

# Find the clod script (using the saved SCRIPT_DIR from the beginning)
CLOD_PATH="$SCRIPT_DIR/clod.sh"

echo -e "\n${YELLOW}Test: Verifying clod.sh exists...${NC}"
if [ ! -f "$CLOD_PATH" ]; then
    echo -e "${RED}✗ clod.sh script not found at $CLOD_PATH${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Found clod.sh at $CLOD_PATH${NC}"
fi

# Create staging directory for test
mkdir -p "$TEST_DIR/staging"

# Run the clod script for the first time with test mode enabled
echo -e "\n${YELLOW}Test: Running clod for the first time...${NC}"
CLOD_TEST_MODE=1 CLOD_TEST_STAGING_DIR="$TEST_DIR/staging" "$CLOD_PATH" 2>&1

# Check if the staging directory exists
STAGING_DIR="$TEST_DIR/staging"
if [ -d "$STAGING_DIR" ]; then
    CLAUDE_UPLOAD_DIR=$(find "$STAGING_DIR" -maxdepth 1 -type d -name "ClaudeUpload_*" | head -n 1)
    echo -e "${GREEN}✓ Staging directory created: $CLAUDE_UPLOAD_DIR${NC}"
else
    echo -e "${RED}✗ Staging directory not created${NC}"
    exit 1
fi

# Check if path manifest exists
MANIFEST_FILE="$CLAUDE_UPLOAD_DIR/_path_manifest.json"
if [ -f "$MANIFEST_FILE" ]; then
    echo -e "${GREEN}✓ Path manifest created${NC}"
else
    echo -e "${RED}✗ Path manifest not created${NC}"
    exit 1
fi

# Check if files were copied and renamed correctly
if [ -f "$CLAUDE_UPLOAD_DIR/README.md" ] && 
   [ -f "$CLAUDE_UPLOAD_DIR/src-utils-math.js" ] && 
   [ -f "$CLAUDE_UPLOAD_DIR/src-components-Header.jsx" ] && 
   [ -f "$CLAUDE_UPLOAD_DIR/src-index.js" ] && 
   [ -f "$CLAUDE_UPLOAD_DIR/test-math.test.js" ]; then
    echo -e "${GREEN}✓ Files were copied and renamed correctly${NC}"
else
    echo -e "${RED}✗ Files were not copied or renamed correctly${NC}"
    ls -la "$CLAUDE_UPLOAD_DIR"
    exit 1
fi

# Check if SVG file was correctly converted to XML
if [ -f "$CLAUDE_UPLOAD_DIR/public-logo-svg.xml" ]; then
    echo -e "${GREEN}✓ SVG file was correctly converted to XML${NC}"
else
    echo -e "${RED}✗ SVG file was not correctly converted to XML${NC}"
    ls -la "$CLAUDE_UPLOAD_DIR"
    exit 1
fi

# Check if binary file was excluded
if [ -f "$CLAUDE_UPLOAD_DIR/binary-file.bin" ]; then
    echo -e "${RED}✗ Binary file was not excluded${NC}"
    exit 1
else
    echo -e "${GREEN}✓ Binary file was correctly excluded${NC}"
fi

# Test manifest content
echo -e "\n${YELLOW}Test: Checking manifest content...${NC}"
MANIFEST_CONTENT=$(cat "$MANIFEST_FILE")
if [[ $MANIFEST_CONTENT == *"README.md"* ]] && 
   [[ $MANIFEST_CONTENT == *"src/utils/math.js"* ]] && 
   [[ $MANIFEST_CONTENT == *"src/components/Header.jsx"* ]] && 
   [[ $MANIFEST_CONTENT == *"public/logo.svg"* ]]; then
    echo -e "${GREEN}✓ Manifest contains correct path mappings${NC}"
else
    echo -e "${RED}✗ Manifest content is incorrect${NC}"
    echo "$MANIFEST_CONTENT"
    exit 1
fi

# Test incremental updates
echo -e "\n${YELLOW}Test: Testing incremental updates...${NC}"

# Commit current files to ensure clean test state
git add .
git commit -m "Commit before modification" > /dev/null

# Modify a file
echo "// Added a comment" >> src/index.js
sleep 1  # Ensure file timestamp is updated

# Run clod again with test mode
CLOD_TEST_MODE=1 CLOD_TEST_STAGING_DIR="$TEST_DIR/staging" "$CLOD_PATH" 2>&1

# Find the new upload directory
NEW_UPLOAD_DIR=$(find "$STAGING_DIR" -maxdepth 1 -type d -name "ClaudeUpload_*" | sort | tail -n 1)

if [ "$NEW_UPLOAD_DIR" != "$CLAUDE_UPLOAD_DIR" ]; then
    echo -e "${GREEN}✓ New staging directory created for incremental update${NC}"
else
    echo -e "${RED}✗ Failed to create new staging directory${NC}"
    exit 1
fi

# Check if only the modified file was included
FILE_COUNT=$(find "$NEW_UPLOAD_DIR" -type f -not -name "_path_manifest.json" -not -name "README.txt" | wc -l)
if [ "$FILE_COUNT" -eq 1 ] && [ -f "$NEW_UPLOAD_DIR/src-index.js" ]; then
    echo -e "${GREEN}✓ Only modified file was included in incremental update${NC}"
else
    echo -e "${RED}✗ Incremental update didn't work correctly${NC}"
    echo "Found $FILE_COUNT files instead of 1:"
    find "$NEW_UPLOAD_DIR" -type f -not -name "_path_manifest.json" -not -name "README.txt"
    exit 1
fi

# Test platform detection
echo -e "\n${YELLOW}Test: Checking platform detection...${NC}"
PLATFORM_CHECK=$(grep "PLATFORM=\"\$(uname -s)\"" "$CLOD_PATH")
if [ -n "$PLATFORM_CHECK" ]; then
    echo -e "${GREEN}✓ Platform detection is present${NC}"
else
    echo -e "${RED}✗ Platform detection is missing${NC}"
    exit 1
fi

# Clean up
echo -e "\n${YELLOW}Cleaning up test directory...${NC}"
cd /tmp
rm -rf "$TEST_DIR"

echo -e "\n${GREEN}All tests passed successfully!${NC}"