# Dhall Serialization Patterns

This document contains patterns and best practices for using Dhall in the project for configuration and serialization.

## Dhall Overview

Dhall is a programmable configuration language that provides:

- A strongly-typed, total, purely functional language
- Type safety with an expressive type system
- Incremental evaluation of configuration
- Ability to import and reuse configuration components

## Basic Serialization

### Type Definitions and Instances

```haskell
-- Basic configuration type with Dhall instances
data Config = Config
  { configPath :: !FilePath
  , configValue :: !Int
  , configEnabled :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)
```

### Loading Configuration

```haskell
import qualified Dhall
import qualified Data.Text as T

-- Load configuration from Dhall file
loadConfig :: FilePath -> IO Config
loadConfig path = do
  -- Convert FilePath (String) to Text for Dhall input
  Dhall.inputFile Dhall.auto (T.pack path)
```

### Error Handling

```haskell
import Control.Exception (SomeException, catch)

-- Gracefully handle parsing errors with defaults
loadConfigSafe :: FilePath -> IO Config
loadConfigSafe path = do
  (Dhall.inputFile Dhall.auto (T.pack path) :: IO Config) 
    `catch` \(_ :: SomeException) -> do
      putStrLn "Warning: Could not load config, using defaults"
      return defaultConfig

-- Default configuration
defaultConfig :: Config
defaultConfig = Config
  { configPath = "default/path"
  , configValue = 42
  , configEnabled = False
  }
```

## Complex Serialization

### Wrapper Types for Collections

```haskell
-- Main data structure with complex types
data ClodDatabase = ClodDatabase
  { dbFiles :: !(Map.Map FilePath FileEntry)
  , dbChecksums :: !(Map.Map String FilePath)
  , dbLastStagingDir :: !(Maybe FilePath)
  , dbLastRunTime :: !UTCTime
  } deriving stock (Show, Eq)

-- Serialization-friendly version
data SerializableClodDatabase = SerializableClodDatabase
  { serializedFiles :: ![(FilePath, FileEntry)]
  , serializedChecksums :: ![(String, FilePath)]
  , serializedLastStagingDir :: !(Maybe FilePath)
  , serializedLastRunTime :: !UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromDhall, ToDhall)
    
-- Convert to serializable form
toSerializable :: ClodDatabase -> SerializableClodDatabase
toSerializable db = SerializableClodDatabase
  { serializedFiles = Map.toList (dbFiles db)
  , serializedChecksums = Map.toList (dbChecksums db)
  , serializedLastStagingDir = dbLastStagingDir db
  , serializedLastRunTime = dbLastRunTime db
  }

-- Convert from serializable form
fromSerializable :: SerializableClodDatabase -> ClodDatabase
fromSerializable sdb = ClodDatabase
  { dbFiles = Map.fromList (serializedFiles sdb)
  , dbChecksums = Map.fromList (serializedChecksums sdb)
  , dbLastStagingDir = serializedLastStagingDir sdb
  , dbLastRunTime = serializedLastRunTime sdb
  }
```

### Composite Type Handling

```haskell
-- Format UTCTime for Dhall (as a record with date, time, timeZone fields)
formatUTCTimeDhall :: UTCTime -> String
formatUTCTimeDhall time =
  let timeStr = show time
      (dateStr, timeWithZone) = span (/= ' ') timeStr
      timeStr' = drop 1 $ takeWhile (/= 'U') (drop 1 timeWithZone)
  in "{ date = \"" ++ dateStr ++ 
     "\", time = \"" ++ timeStr' ++ 
     "\", timeZone = \"UTC\" }"
```

### Saving to Dhall Format

```haskell
-- Save database to Dhall format
saveDatabase :: FilePath -> ClodDatabase -> IO ()
saveDatabase path db = do
  let serializable = toSerializable db
  
  -- For complex types, manual construction can be more reliable
  let dhallText = T.pack $ 
        "{ serializedFiles = " ++
        formatEntries (serializedFiles serializable) ++
        ", serializedChecksums = " ++ 
        formatChecksums (serializedChecksums serializable) ++
        ", serializedLastStagingDir = " ++
        formatMaybe (serializedLastStagingDir serializable) ++
        ", serializedLastRunTime = " ++
        formatUTCTimeDhall (serializedLastRunTime serializable) ++
        "}\n"
  
  -- Write to temp file first for atomic updates
  TextIO.writeFile (path ++ ".tmp") dhallText
  renameFile (path ++ ".tmp") path
  
  -- Helper functions for formatting
  where
    formatEntries [] = "[] : List { _1 : Text, _2 : FileEntry }"
    formatEntries entries = 
      "[\n  " ++ intercalate ",\n  " [formatEntry e | e <- entries] ++ "\n]"
    
    formatEntry (path, entry) = 
      "{ _1 = \"" ++ escape path ++ "\", _2 = " ++ formatFileEntry entry ++ " }"
    
    formatChecksums [] = "[] : List { _1 : Text, _2 : Text }"
    formatChecksums checksums =
      "[\n  " ++ intercalate ",\n  " [formatChecksum c | c <- checksums] ++ "\n]"
    
    formatChecksum (hash, path) =
      "{ _1 = \"" ++ escape hash ++ "\", _2 = \"" ++ escape path ++ "\" }"
    
    formatMaybe Nothing = "None Text"
    formatMaybe (Just s) = "Some \"" ++ escape s ++ "\""
    
    escape = concatMap escapeChar
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]
```

### Type Annotations for Empty Lists

```haskell
-- Empty lists need explicit type annotations in Dhall
formatEmptyList :: String -> String
formatEmptyList typeName = "[] : List " ++ typeName

-- Examples
emptyFiles = "[] : List { _1 : Text, _2 : FileEntry }"
emptyChecksums = "[] : List { _1 : Text, _2 : Text }"
```

## Dhall Configuration Examples

### Simple Configuration

```dhall
-- config.dhall
{ configPath = "./data"
, configValue = 42
, configEnabled = True
}
```

### List Configuration

```dhall
-- file_types.dhall
{ 
  textExtensions = 
    [ -- Documentation
      ".txt", ".text", ".md", ".markdown", ".csv", ".tsv"
      -- Markup  
    , ".html", ".htm", ".xhtml", ".xml", ".svg", ".rss"
    ]
, binaryExtensions =
    [ -- Images
      ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".ico"
      -- Archives
    , ".zip", ".tar", ".gz", ".7z", ".rar"
    ]
}
```

### Custom Types and Records

```dhall
-- binary_signatures.dhall
-- Define a type for binary signatures
let Signature = { name : Text, bytes : List Natural }

let signatures : List Signature =
    [ { name = "JPEG", bytes = [0xFF, 0xD8, 0xFF] }
    , { name = "PNG", bytes = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A] }
    , { name = "GIF", bytes = [0x47, 0x49, 0x46, 0x38] }
    ]

in { signatures = signatures }
```

## Best Practices

### Defensive Loading

```haskell
-- Try to load config, fall back to defaults if anything fails
loadDatabaseSafe :: FilePath -> IO ClodDatabase
loadDatabaseSafe dbPath = do
  fileExists <- doesFileExist dbPath
  if not fileExists
    then do
      -- Create a new database
      db <- initializeDatabase
      saveDatabase dbPath db
      return db
    else do
      -- Try to load, with error handling
      (do
        sdb <- Dhall.inputFile Dhall.auto (T.pack dbPath) :: IO SerializableClodDatabase
        return $ fromSerializable sdb)
        `catch` \(e :: SomeException) -> do
          putStrLn $ "Warning: Could not parse database: " ++ show e
          -- Create a new database
          db <- initializeDatabase
          saveDatabase dbPath db
          return db
```

### Cache Configuration

```haskell
-- This is safer than it looks because configuration loading is idempotent
-- and the result is referentially transparent
getConfig :: Config
getConfig = unsafePerformIO $ do
  loadConfigSafe defaultConfigPath
{-# NOINLINE getConfig #-}
```

### Dhall in Cabal

```
data-files:
  resources/config.dhall,
  resources/file_types.dhall,
  resources/binary_signatures.dhall
```

### Loading Data Files

```haskell
import Paths_clod (getDataFileName)

loadFileTypes :: IO FileTypes
loadFileTypes = do
  -- Use getDataFileName to find resource in installed package
  path <- getDataFileName "resources/file_types.dhall"
  (Dhall.input Dhall.auto (T.pack path) :: IO FileTypes) `catch` \(_ :: SomeException) -> 
    pure defaultFileTypes
```

## Common Pitfalls

### Record Field Ordering

Dhall records don't require fields in any specific order, but the field names must match exactly:

```haskell
-- Dhall type
data Config = Config
  { fieldA :: String
  , fieldB :: Int
  }

-- This Dhall is valid even though fields are in different order
-- { fieldB = 42, fieldA = "value" }
```

### Type Annotations for Empty Collections

Always provide type annotations for empty collections:

```dhall
-- This will fail
{ emptyList = [] }

-- This will work
{ emptyList = [] : List Text }
```

### Handling Complex Types

For types like UTCTime that Dhall doesn't natively support, use record representations:

```dhall
-- UTCTime representation
{ date = "2023-10-15"
, time = "14:30:45.789012"
, timeZone = "UTC"
}
```

### Escaping String Values

Remember to escape special characters in strings:

```haskell
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]
```