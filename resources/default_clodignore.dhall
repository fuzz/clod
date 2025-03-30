-- Default ignore patterns for Clod uploader
-- These patterns specify which files to ignore when uploading to Claude

{ 
  -- Default ignore patterns grouped by type
  ignorePatterns = 
    [ 
      -- Binary and media files
      "*.dll" : Text
    , "*.dylib" : Text
    , "*.exe" : Text
    , "*.gif" : Text
    , "*.ico" : Text
    , "*.jar" : Text
    , "*.jpg" : Text
    , "*.jpeg" : Text
    , "*.mp3" : Text
    , "*.mp4" : Text
    , "*.png" : Text
    , "*.so" : Text
    , "*.svg" : Text
    , "*.tar.gz" : Text
    , "*.zip" : Text
      
      -- Build directories
    , ".clod" : Text
    , ".git" : Text
    , "build" : Text
    , "dist" : Text
    , "node_modules" : Text
    , "out" : Text
    , "target" : Text
      
      -- Large files and lock files
    , "*.log" : Text
    , "Cargo.lock" : Text
    , "package-lock.json" : Text
    , "pnpm-lock.yaml" : Text
    , "yarn.lock" : Text
      
      -- Configuration files
    , ".gitignore" : Text
    , ".clodignore" : Text
    ]
}