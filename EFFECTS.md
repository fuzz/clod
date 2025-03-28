# Effects System for Clod

This document explains the capability-based effects system used in Clod to improve safety when working with AI-generated code.

## Overview

Clod uses algebraic effects through the Polysemy library to provide a capability-based security model for file system operations and Git interactions. This enables an extra layer of safety when working with AI-generated code by restricting file access to explicitly allowed directories.

## Core Concepts

### 1. Effects

Effects are explicitly tracked in type signatures using the `Members` constraint, making it clear what side effects a function can have:

```haskell
processFileWithEffects :: Members '[FileSystem, Error T.ClodError, Console, Reader T.ClodConfig] r
                       => FileReadCap -> FileWriteCap -> FilePath -> FilePath -> Sem r T.FileResult
```

This type signature indicates that the function can:
- Perform file system operations (`FileSystem`)
- Throw typed errors (`Error ClodError`)
- Write to the console (`Console`)

But it cannot:
- Access the network
- Launch external processes
- Perform other unrestricted IO

### 2. Capabilities

Capabilities are "permission tokens" that grant access to specific resources. In Clod, we use capabilities to restrict file access to specific directories:

```haskell
data FileReadCap = FileReadCap { allowedReadDirs :: [FilePath] }
data FileWriteCap = FileWriteCap { allowedWriteDirs :: [FilePath] }
data GitCap = GitCap { allowedRepos :: [FilePath] }
```

Functions that need to access files must be explicitly granted these capabilities:

```haskell
safeReadFile :: Members '[FileSystem, Error ClodError] r 
             => FileReadCap -> FilePath -> Sem r ByteString
```

This ensures that even if AI-generated code tries to access files outside the allowed directories, it will be blocked by the capability system.

## Using the Effects System

### 1. Default Behavior

Clod uses the effects system by default with safe permissions. The capability-based approach restricts file access to:

- The repository directory for reading (where Clod is run)
- The staging directory for writing (where processed files are stored)
- The repository directory for Git operations

### 2. Programmatic Usage

In your Haskell code, you can use the effects system like this:

```haskell
import Polysemy
import Clod.Effects
import Clod.Capability

myFunction :: Members '[FileSystem, Console] r 
           => FileReadCap -> FileWriteCap -> Sem r ()
myFunction readCap writeCap = do
  -- Only allowed to read files in directories specified by readCap
  content <- safeReadFile readCap "allowed/file.txt"
  
  -- Only allowed to write files in directories specified by writeCap
  safeWriteFile writeCap "allowed/output.txt" processedContent
```

### 3. Example Code

See `examples/EffectsExample.hs` for a complete demonstration of how the effects system protects against malicious or buggy AI-generated code.

## Benefits for AI Safety

1. **Path Restriction**: AI can only access files in explicitly permitted directories
2. **Type Safety**: Effects are enforced by the type system at compile time
3. **Intent Signaling**: Functions that need file access must explicitly request capabilities
4. **Principle of Least Privilege**: AI code only gets the minimal permissions it needs

## Technical Details

### Effect Implementation

Effects are defined using Polysemy's GADT-based approach:

```haskell
data FileSystem m a where
  ReadFile :: FilePath -> FileSystem m ByteString
  WriteFile :: FilePath -> ByteString -> FileSystem m ()
  CopyFile :: FilePath -> FilePath -> FileSystem m ()
  FileExists :: FilePath -> FileSystem m Bool
```

Each effect has a corresponding interpreter that can run the effect in different contexts:

```haskell
runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
```

### Capability Checking

Capabilities are checked at runtime by verifying path prefixes:

```haskell
isPathAllowed :: [FilePath] -> FilePath -> Bool
isPathAllowed allowedDirs path = any (\dir -> dir `isPrefixOf` path) allowedDirs
```

This simple but effective approach ensures that files can only be accessed within the allowed directories.

## Future Enhancements

Future versions may include:
- More fine-grained capabilities (e.g., per-file permissions)
- Static analysis to verify capability usage at compile time
- Integration with more advanced effect types like `Eff` or `fused-effects`
- Property-based testing of capability invariants