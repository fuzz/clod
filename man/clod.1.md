% CLOD(1) Clod 0.1.4
% Fuzz Leonard
% March 2025

# NAME

clod - Claude Loader for preparing files for Claude AI's Project Knowledge

# SYNOPSIS

**clod** [*OPTIONS*]

# DESCRIPTION

Clod is a utility for preparing and uploading files to Claude AI's Project Knowledge feature. 
It tracks file changes, respects .gitignore and .clodignore patterns, and optimizes filenames 
for Claude's UI.

# OPTIONS

**--all**, **-a**
: Process all files (respecting .gitignore and .clodignore)

**--test**, **-t**
: Run in test mode (no prompts, useful for CI)

**--staging-dir** *DIR*, **-d** *DIR*
: Specify a directory for test mode (only used with --test)

**--verbose**, **-v**
: Enable verbose output

**--flush**, **-f**
: Remove missing entries from the database

**--last**, **-l**
: Reuse the previous staging directory

**--help**
: Show help information

**--version**
: Show version information

# EXAMPLES

Run clod (first run processes all files, subsequent runs process only modified files):
    clod

Force processing of all files:
    clod --all

Run in test mode with an optional test directory:
    clod --test --staging-dir /path/to/test/dir
    
Reuse the previous staging directory:
    clod --last
    
Remove missing entries from the database:
    clod --flush

# ENVIRONMENT VARIABLES

**CLOD_DIR**
: Override the default .clod directory name

**CLODIGNORE**
: Override the default .clodignore filename

# FILES

**.clodignore**
: Pattern file similar to .gitignore for excluding files

**.clod/database.dhall**
: Database of file checksums and metadata

# SEE ALSO

**clod(7)** for information about project instructions and safeguards.
**clod(8)** for a complete workflow guide to using clod with Claude AI.
