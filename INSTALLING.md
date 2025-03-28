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

### Automatic Installation

After installing clod, you can easily install the man pages:

```bash
# Clone the repository if you haven't already
git clone https://github.com/fuzz/clod.git
cd clod

# Generate and install man pages
./bin/install-man-pages.sh
```

The script will:
1. Generate the man pages
2. Install them to `~/.config/clod/man` (using XDG config directory specification)
3. Detect your shell and provide instructions for adding this location to your `MANPATH` 
4. Explain how to view the man pages

### Manual Installation

If you prefer, you can install the man pages manually:

```bash
# Generate the man pages
cd path/to/clod/repo
./bin/generate-man-pages.sh

# Create directories
mkdir -p ~/.config/clod/man/man1
mkdir -p ~/.config/clod/man/man7
mkdir -p ~/.config/clod/man/man8

# Copy the man pages
cp man/clod.1 ~/.config/clod/man/man1/
cp man/clod.7 ~/.config/clod/man/man7/
cp man/clod.8 ~/.config/clod/man/man8/

# Add to your shell configuration (choose based on your shell)
echo 'export MANPATH=$MANPATH:~/.config/clod/man' >> ~/.bashrc  # For bash
echo 'export MANPATH=$MANPATH:~/.config/clod/man' >> ~/.zshrc   # For zsh
echo 'set -x MANPATH $MANPATH ~/.config/clod/man' >> ~/.config/fish/config.fish  # For fish
```

## Viewing Man Pages

After installation and updating your `MANPATH`, you can view the man pages with:

```bash
man 1 clod  # Command reference
man 7 clod  # Project instructions
man 8 clod  # Workflow guide
```

Or simply:

```bash
man clod  # Shows the main command reference (section 1)
```

If you haven't updated your `MANPATH`, you can still view the man pages using the full path:

```bash
man -M ~/.config/clod/man 1 clod
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