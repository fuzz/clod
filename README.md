# clod

A streamlined workflow system for coding with Claude AI using filesystem access and project knowledge.

## Comparison with Claude Code

While Anthropic's Claude Code offers powerful agentic capabilities directly in your terminal, clod provides a complementary and often more cost-effective approach:

### When to Use clod vs. Claude Code

- **Cost Efficiency**: clod leverages Claude Pro's project knowledge caching, resulting in significantly lower token usage compared to Claude Code's real-time analysis.
- **Hybrid Approach**: I find success using clod with Claude Pro as my primary workflow, switching to Claude Code only when hitting Pro plan limits.
- **Test Integration**: When combined with file watching tools like fswatch (see below), clod offers comparable testing capabilities to Claude Code at a fraction of the token cost.
- **Seamless Fallback**: If you reach Claude Pro limits, you can continue your work with Claude Code until access is restored without changing your workflow significantly.

The ideal workflow for me is:
1. Use clod with Claude Pro for day-to-day development tasks
2. Set up fswatch or similar tools for automated testing
3. Keep Claude Code as a backup for high-volume days or especially complex tasks requiring whole-codebase analysis

This hybrid approach optimizes both cost and capability while ensuring continuous productivity.

## The Problem

When working with code in Claude, you face several challenges:

1. **Project Knowledge Management**: The project knowledge section in Claude accepts files, but with several limitations:
   - Limited filename display (long paths get truncated)
   - Duplicate filenames are hard to distinguish
   - No direct connection to your local file system
   
2. **Workflow Friction**: Moving files between your local environment and Claude involves multiple manual steps:
   - Selecting which files to upload
   - Uploading them to Claude's project knowledge
   - Remembering original paths when writing changes back
   - Managing incremental updates as you modify files

3. **Context Window Costs**: Using Claude's filesystem access directly on all files can quickly consume your context window, significantly limiting conversation length.

## The Solution

clod provides a complete, end-to-end workflow for coding with Claude AI:

1. **Smart File Selection & Upload Preparation**:
   - A bash script that finds modified files in your git repository
   - Respects `.gitignore` patterns and excludes binary files
   - Optimizes filenames for Claude's UI (converting paths to prefixes)
   - Creates a path manifest for accurately writing files back
   
2. **Seamless Code Modification Workflow**:
   - Project instructions that teach Claude how to use the uploaded files
   - Automatic path resolution when writing files back to disk
   - End-to-end implementation of requested changes with minimal user input
   
3. **Testing Integration**:
   - Proactive test coverage for modified code
   - Automatic test updates alongside code changes

## Installation

1. Clone this repository:
```bash
git clone https://github.com/fuzz/clod.git
```

2. Add the script to your PATH:
```bash
cp clod/clod.sh ~/bin/clod
chmod +x ~/bin/clod
```

## Usage

### Preparing Files for Claude

Run the script from within your git repository:

```bash
cd your-project
clod
```

On first run, you'll be prompted to:
1. Import all files
2. Import only modified files
3. Just set the timestamp for future incremental updates

The script creates a staging directory at `~/Claude/ClaudeUpload_TIMESTAMP/` containing:
- Your code files with optimized names
- A `_path_manifest.json` file mapping optimized names to original paths
- A README.txt explaining the contents

### Uploading to Claude

1. Start a new Claude Project
2. Open the Project Knowledge section
3. Drag and drop the files from the staging directory
4. Upload the project instructions from this repository to set up the workflow

### Making Changes

Once your files and instructions are uploaded, simply tell Claude what you want to change:

```
"Please update the header component to use our new brand colors #1A2B3C for the background and #EAEAEA for the text"
```

Claude will:
1. Identify which files need to be changed
2. Make the necessary modifications
3. Generate artifacts showing you the changes
4. Write the modified files back to your local filesystem
5. Update or create tests as needed

## Automatic Testing with File Watching

clod works even better when combined with file watching tools that automatically run tests when Claude writes changes back to your filesystem.

> **Note:** Currently, Claude's filesystem access is only available on macOS and Windows desktop applications, not on Linux. The file watching setup below is applicable only for macOS.

### Using fswatch for Automatic Testing

Here's a basic example of using [fswatch](https://github.com/emcrisostomo/fswatch) to automatically run tests for a Node.js project:

```bash
# Install fswatch
brew install fswatch

# Create a simple watcher script
cat > test-watcher.sh << 'EOF'
#!/bin/bash
# Simple test watcher for Node.js projects

# Path to your project
PROJECT_PATH="$1"
if [ -z "$PROJECT_PATH" ]; then
  echo "Usage: $0 /path/to/your/project"
  exit 1
fi

run_tests() {
  echo "🧪 Running tests at $(date)"
  cd "$PROJECT_PATH" || exit 1
  
  # Only run tests if package.json exists
  if [ -f "package.json" ]; then
    npm test
  else
    echo "No package.json found - skipping tests"
  fi
  
  echo "✅ Done"
  echo "---------------------------------"
}

echo "👀 Watching $PROJECT_PATH for changes..."
echo "Press Ctrl+C to stop watching"

# Initial test run
run_tests

# Start watching for file changes
fswatch -o "$PROJECT_PATH" | while read -r; do
  run_tests
done
EOF

# Make it executable
chmod +x test-watcher.sh

# Run it
./test-watcher.sh ~/path/to/your/project
```

This simple setup ensures that as Claude makes changes to your codebase, you'll get immediate feedback on whether those changes maintain the integrity of your project.

## Safety Guardrails

While clod is designed to streamline AI-assisted coding, it's important to implement appropriate safety guardrails for your specific use case.

We've included a separate [guardrails.md](guardrails.md) document with recommended safety practices for:

- API cost management
- Filesystem protection
- Code quality safeguards
- User education

**We strongly recommend reviewing these guardrails and implementing those appropriate for your project before using clod in production environments.**

To get started:
1. Review [guardrails.md](guardrails.md) 
2. Select appropriate guardrails for your project
3. Add them to your project instructions when setting up with Claude

Different teams and projects will have different safety requirements - the guardrails document provides a menu of options to choose from rather than a one-size-fits-all solution.

## Example Workflow

Here's a typical workflow using clod:

1. **Initial Setup**:
   ```bash
   cd my-react-project
   clod  # Choose "Import all files"
   ```

2. **Upload to Claude**:
   - Create a new Claude Project called "My React Project"
   - Upload files from the staging directory to Project Knowledge
   - Upload the project instructions

3. **Request Changes**:
   "Please refactor the user authentication flow to use JWT tokens instead of session cookies"

4. **Review and Approve**:
   - Claude shows you artifacts with modified code
   - Claude explains key changes made
   - You approve the changes

5. **Next Iteration**:
   ```bash
   clod  # Now only shows files modified since last run
   ```

## Configuration

The script creates a configuration directory at `.clod/` in your project root:
- `last-run-marker`: Tracks when the script was last run for incremental updates
- Path mappings are stored in each staging directory

## Requirements

- macOS with Bash shell environment
- Git repository
- Claude Pro or Team account with filesystem access enabled

## Platform Support

Currently, clod is designed and tested for macOS environments. While Claude's filesystem access is available on both macOS and Windows, this tool has only been verified on macOS.

Community contributions to add Windows support are welcome.

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
