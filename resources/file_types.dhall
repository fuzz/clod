-- File type definitions for binary file detection

{ 
  -- Text file extensions
  textExtensions = 
    [ -- Documentation
      ".txt", ".text", ".md", ".markdown", ".csv", ".tsv"
      -- Markup
    , ".html", ".htm", ".xhtml", ".xml", ".svg", ".rss"
      -- Stylesheets
    , ".css", ".scss", ".sass", ".less"
      -- Web
    , ".js", ".jsx", ".ts", ".tsx", ".json", ".yaml", ".yml"
      -- C-style
    , ".c", ".cpp", ".cc", ".h", ".hpp", ".cs"
      -- JVM
    , ".java", ".scala", ".kt", ".groovy"
      -- Scripting
    , ".py", ".rb", ".php", ".pl", ".pm"
      -- Functional Programming
    , ".hs", ".lhs", ".elm", ".purs"
      -- Data
    , ".sql", ".graphql"
      -- Shell
    , ".sh", ".bash", ".zsh", ".bat", ".cmd", ".ps1"
      -- Config
    , ".log", ".conf", ".config", ".ini", ".toml"
      -- Publishing
    , ".tex", ".bib", ".rst"
    ]

  -- Binary file extensions
, binaryExtensions = 
    [ -- Documents
      ".pdf", ".doc", ".docx", ".xls", ".xlsx", ".ppt", ".pptx"
      -- Images
    , ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".webp"
      -- Audio
    , ".mp3", ".mp4", ".wav", ".ogg", ".flac", ".aac", ".m4a"
      -- Video
    , ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm"
      -- Archives
    , ".zip", ".tar", ".gz", ".bz2", ".7z", ".rar", ".jar"
      -- Binaries
    , ".exe", ".dll", ".so", ".dylib", ".bin", ".iso"
      -- Compiled
    , ".class", ".pyc", ".pyo", ".o", ".a", ".obj"
      -- Fonts
    , ".ttf", ".otf", ".woff", ".woff2", ".eot"
      -- Databases
    , ".db", ".sqlite", ".mdb", ".accdb"
      -- Adobe
    , ".psd", ".ai", ".indd", ".eps"
    ]

  -- Special case filenames (always text)
, textSpecialCases =
    [ "Makefile"
    , "Dockerfile"
    , "LICENSE"
    , "README"
    , ".gitignore"
    , ".dockerignore"
    , ".clodignore"
    , ".env"
    , ".gitattributes"
    , "CMakeLists.txt"
    ]

  -- Special case patterns (always binary)
, binarySpecialCases =
    [ ".min.js"
    , ".bundle.js"
    , "node_modules"
    , "build"
    , "dist"
    ]
}