# Installing Clod

## Using Homebrew (macOS)

```bash
# Install from the tap
brew tap fuzz/tap
brew install clod

# Or in one command:
brew install fuzz/tap/clod
```

## Using Cabal (All Platforms)

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

### Generating Man Pages Manually

If you want to generate the man pages manually:

```bash
# Generate man pages in the current directory
bin/generate-man-pages.sh

# Or generate to a specific directory
bin/generate-man-pages.sh /path/to/output/dir
```

This requires pandoc to be installed on your system. The script will create three directories for man page sections (man1, man7, man8) and place the generated man pages in them.

### Installing Man Pages Manually

If you need to install the man pages manually after generating them:

```bash
# System-wide installation (requires admin privileges)
sudo cp man1/clod.1 /usr/local/share/man/man1/
sudo cp man7/clod.7 /usr/local/share/man/man7/
sudo cp man8/clod.8 /usr/local/share/man/man8/

# Or in your home directory
mkdir -p ~/.local/share/man/man1 ~/.local/share/man/man7 ~/.local/share/man/man8
cp man1/clod.1 ~/.local/share/man/man1/
cp man7/clod.7 ~/.local/share/man/man7/
cp man8/clod.8 ~/.local/share/man/man8/
```

## Opening the Staging Directory (macOS)

On macOS, you can directly open the staging directory using the `open` command with the output from clod:

```bash
# Run clod and open the staging directory in Finder
open `clod`

# For scripts, you can capture the output and open it
STAGING_DIR=$(clod)
open "$STAGING_DIR"
```

For other platforms, you can use similar commands appropriate for your operating system, or create a simple wrapper script if needed.

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
- libmagic (required for file type detection)
  - On macOS: `brew install libmagic`
  - On Linux: `apt-get install libmagic-dev` or equivalent for your distribution
  - On Windows: Install from source or use package manager
- pandoc (optional, for generating man pages)
  - On macOS: `brew install pandoc`
  - On Linux: `apt-get install pandoc` or equivalent for your distribution
  - On Windows: Install via package manager or from official website

## Troubleshooting

### Missing Man Pages

If you can't access the man pages after installation:

1. Check if pandoc was available during installation (`brew info clod` to check for Homebrew)
2. Generate and install the man pages manually using the instructions above
3. Verify your `MANPATH` environment variable includes the installation directory
4. On some systems, you may need to run `mandb` to update the man page database

### libmagic Issues

If you encounter problems with libmagic:

1. Ensure libmagic is properly installed
2. Check that the dynamic library is in your library path
3. On macOS, you might need to set `DYLD_LIBRARY_PATH` to include the libmagic location
