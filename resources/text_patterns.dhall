-- Text patterns for content-based file type detection
-- These patterns are used to identify text files by analyzing their content description

{ 
  -- Patterns that indicate text content (case-insensitive)
  textPatterns = 
    [ "text" : Text
    , "ascii" : Text
    , "utf" : Text
    , "unicode" : Text
    , "empty" : Text  -- Empty files are considered text
      
      -- Common markup formats
    , "html" : Text
    , "xml" : Text
    , "json" : Text
    , "yaml" : Text
    , "csv" : Text
    , "markdown" : Text
      
      -- Programming and scripting
    , "script" : Text
    , "source" : Text
    , "program" : Text
    , "code" : Text
      
      -- Document formats
    , "document text" : Text  -- Use more specific pattern to avoid matching "PDF document"
    , "manuscript" : Text
    , "mail" : Text
    , "email" : Text
    , "rfc" : Text
    
      -- Configuration
    , "config" : Text
    , "conf" : Text
    , "settings" : Text
    
      -- Web-related
    , "stylesheet" : Text
    , "css" : Text
    , "javascript" : Text
    
      -- Shell scripts
    , "shell" : Text
    , "bash" : Text
    , "perl" : Text
    , "python" : Text
    , "ruby" : Text
    , "php" : Text
    , "haskell" : Text
    ]
}