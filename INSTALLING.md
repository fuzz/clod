# Installing Clod

Clod can be installed using Cabal:

```bash
cabal install clod
```

## Man Pages

Clod includes comprehensive documentation as man pages:

- `clod(1)`: Command usage and options
- `clod(7)`: Project instructions and safeguards
- `clod(8)`: Complete workflow guide for using clod with Claude AI

### Installation

When installing clod through a package manager or Hackage, the man pages are automatically installed to the system's standard man page location:

- **Homebrew**: Man pages are installed to `/opt/homebrew/share/man/` (or equivalent)
- **Hackage/Cabal**: Man pages are included in the standard package installation

No additional configuration is required to use these man pages - they're automatically accessible through the `man` command.

## Using the Wrapper

Clod includes a wrapper executable (`cld`) that runs clod and automatically opens the staging directory in your file browser. When you install clod with `cabal install`, both the main program and the wrapper are installed automatically.

```bash
# Use the wrapper to run clod and automatically open the staging directory
cld [options]

# Run the wrapper without opening the staging directory
cld --no-open [options]
```

## Viewing Man Pages

After installation, you can view the man pages with:

```bash
man 1 clod  # Command reference
man 7 clod  # Project instructions
man 8 clod  # Workflow guide
```

Or simply:

```bash
man clod  # Shows the main command reference (section 1)
```

## Prerequisites

- GHC (Glasgow Haskell Compiler) 9.0 or newer
- libgit2 (required for Git operations)
  - On macOS: `brew install libgit2`
  - On Linux: `apt-get install libgit2-dev` or equivalent for your distribution
  - On Windows: Install from source or use package manager
- pandoc (required for generating man pages)
  - On macOS: `brew install pandoc`
  - On Linux: `apt-get install pandoc` or equivalent for your distribution
  - On Windows: Install via package manager or from official website