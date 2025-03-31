# Default ignore patterns for Clod uploader
# These patterns specify which files to ignore when uploading to Claude

# Binary and media files
*.dll
*.dylib
*.exe
*.gif
*.ico
*.jar
*.jpg
*.jpeg
*.mp3
*.mp4
*.png
*.so
*.svg
*.tar.gz
*.zip

# Build directories
.clod
.git
build
dist
node_modules
out
target

# Large files and lock files
*.log
Cargo.lock
package-lock.json
pnpm-lock.yaml
yarn.lock

# Configuration files
.gitignore
.clodignore