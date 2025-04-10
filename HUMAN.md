# clod: Human Guide

A streamlined workflow system for coding with the Claude desktop app using Filesystem Access and Project Knowledge

## What is clod?

clod (meat-robot hybrid) creates a smooth integration between your local codebase and Claude AI's coding capabilities. It solves key problems when using Claude for coding:

1. It optimizes your files for Claude's project knowledge UI
2. It tracks which files have changed since your last upload
3. It maintains a mapping between Claude's filenames and your actual repository paths
4. It provides clear instructions to Claude on how to implement your requests

## Claude Features Used by clod

**Project Knowledge** is a feature in Claude that allows you to upload files that Claude can reference during your conversation. These files remain accessible throughout your project without consuming your conversation's context window.

**Filesystem Access** is a feature that allows Claude to read from and write to files on your local system (currently available only on macOS and Windows desktop applications). This enables Claude to directly modify your codebase based on your instructions.

## Prerequisites

- **Claude Pro, Max, Teams or Enterpise account** with access to:
- **Project Knowledge** - Claude's file storage system that keeps files available throughout your project
- **Filesystem Access** - Claude's ability to read and write files on your computer (currently available only on macOS and Windows desktop apps)

## Comparison with Claude Code

While Anthropic's Claude Code offers powerful agentic capabilities directly in your terminal, clod provides a complementary and often more cost-effective approach:

### When to Use clod vs. Claude Code

- **Cost Efficiency**: clod leverages Claude Pro's project knowledge caching, resulting in significantly lower token usage compared to Claude Code's real-time analysis.
- **Hybrid Approach**: A typical workflow for me is to work with Claude App on a new feature for a few iterations, until he gets stuck trying to get a test to pass or something. Then I hand it over to Claude Code and let him solve the final problems with local access and also have him double-check the other Claude’s work. I say “save 50%” but really I probably save more like 90% with this approach.
- **Seamless Fallback**: If you reach Claude App limits, you can continue your work with Claude Code until access is restored without changing your workflow significantly. Now that Claude Max has been released it may be more cost effective to upgrade your Claude App plan, depending on your use case.

The workflow I use is:
1. Use clod with Claude App for most day-to-day development tasks--he's a
   better conversationalist than Claude Code, anyway, plus he can render visual
   elements for live previews
2. Use fswatch for automated testing
3. Use Claude Code for final code fixes and code review after iterating with Claude App, as a backup for high-volume days, for especially complex tasks requiring whole-codebase analysis or exploration with command-line tools

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
   - Haskell-based tool that finds modified files in your git repository
   - Respects `.gitignore` patterns and excludes binary files
   - Optimizes filenames for Claude's UI (converting paths to prefixes)
   - Creates the _path_manifest.dhall file for accurately mapping filenames back to original paths
   
2. **Seamless Code Modification Workflow**:
   - Project instructions that teach Claude how to use the uploaded files
   - Automatic path resolution when writing files back to disk
   - End-to-end implementation of requested changes with minimal user input
   
3. **Testing Integration**:
   - Proactive test coverage for modified code
   - Automatic test updates alongside code changes

## Special File Handling

clod includes special handling for certain file types to ensure optimal compatibility with Claude's Project Knowledge system.

### Hidden Files and Directories

Hidden files and directories (those starting with a `.` character) are transformed to ensure visibility in file browsers when uploading to Claude.

#### How Hidden File Handling Works

1. When clod processes hidden files or directories, it transforms them with a consistent format:
   - `.gitignore` becomes `dot--gitignore`
   - `.env` becomes `dot--env`
   - `.config/settings.ini` becomes `dot--config-settings.ini`

2. The original file path with the leading dot is preserved in the `_path_manifest.dhall` file, ensuring Claude writes back to the correct location with the proper hidden file format.

3. In your conversations with Claude, you can refer to these files using either name:
   - "Can you modify the .env file?"
   - "Can you update the dot--env file?"

4. When Claude writes the file back to your filesystem, it will use the original path with the leading dot.

#### Benefits

- Hidden files are visible in macOS Finder and Windows Explorer when selecting files to upload
- Consistent naming convention makes hidden files easy to identify
- No manual renaming is needed - transformation happens automatically
- Your project structure remains clean with proper dot-prefixed files
- Hidden directories within paths are also properly transformed

### SVG Files

SVG files are automatically converted to XML files when processed by clod. This is because Claude's Project Knowledge system doesn't officially support the SVG file extension, but it can work with XML files (since SVGs are fundamentally XML files).

#### How SVG Handling Works

1. When clod processes an SVG file, it renames it with a special format:
   - `logo.svg` becomes `logo-svg.xml`
   - `public/logo.svg` becomes `public-logo-svg.xml`

2. The original file path is preserved in the `_path_manifest.dhall` file, ensuring Claude writes back to the correct SVG file when making changes.

3. In your conversations with Claude, you can refer to these files using either name:
   - "Can you modify the SVG in public/logo.svg?"
   - "Can you update the XML in public-logo-svg.xml?"

4. When Claude writes the file back to your filesystem, it will use the original SVG path.

#### Benefits

- You can continue working with standard SVG files in your projects without interruption
- No manual conversion is needed - everything happens automatically
- Claude can fully view and edit SVG content just like any other XML file
- Your project structure remains clean with proper SVG extensions

This feature allows you to leverage Claude's capabilities with SVG files while ensuring compatibility with the Project Knowledge system.

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
   - Click on "Project Instructions" in the left sidebar
   - Paste the contents of `project-instructions.md` into this section
   - Add any desired guardrails to the bottom of the Project Instructions
   - Start a new conversation

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
   - Upload the new files from the staging directory
   - **Important**: Before starting a new conversation, manually delete the previous versions of these files from Project Knowledge
   - Start a new conversation

## Working with Project Knowledge

When working with Claude on complex codebases, you may sometimes notice that Claude doesn't fully consider all files in the project knowledge section. This is due to how Claude's Retrieval-Augmented Generation (RAG) works with large file collections.

### Tips for Better File Retrieval

1. **Be specific about file references**: If Claude seems to miss context, explicitly mention the relevant files:
   ```
   "Please check the file config-settings.js in the project knowledge section to see how we handle environment variables."
   ```

2. **Prompt thorough examination**: Encourage Claude to thoroughly check all relevant files:
   ```
   "Before implementing this change, please carefully consider all files in the project knowledge section that relate to user authentication."
   ```

3. **Confirm file content understanding**: Ask Claude to summarize key files to ensure proper context:
   ```
   "Could you first summarize what our current Header component does based on the file in project knowledge?"
   ```

4. **Guide file exploration**: If working with a large codebase, guide Claude's attention:
   ```
   "The relevant code is primarily in the src/components and src/utils directories. Please focus on those files first."
   ```

5. **Iterative refinement**: If Claude misses important context, point it out explicitly:
   ```
   "I notice you didn't consider how this interacts with the API client in api-client.js. Please review that file and adjust your implementation."
   ```

These techniques can significantly improve Claude's ability to work effectively with your codebase.

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

### Using Test Results with Claude

When using file watchers to run tests:

1. After Claude makes changes to your code, the file watcher will automatically run tests
2. Share test output with Claude by copying the terminal output
3. Claude can analyze test failures and suggest fixes
4. This creates a rapid feedback loop where Claude can iteratively improve the code

This simple setup ensures that as Claude makes changes to your codebase, you'll get immediate feedback on whether those changes maintain the integrity of your project.

## Configuration

The tool creates a configuration directory at `.clod/` in your project root:
- `db.dhall`: Tracks checksums, filename mappings and the location of the
  previous staging directory
- Path mappings are stored in each staging directory
