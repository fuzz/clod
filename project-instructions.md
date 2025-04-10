# Project Instructions
*These instructions should be pasted into the Project Instructions section of your Claude Project.*
*Human readers: For more detailed explanations, see HUMAN.md file*

## Overview

This project enables seamless code editing by combining Claude's filesystem access with project knowledge. You request changes in natural language, and Claude handles all implementation details from file lookup to writing changes back to disk.

## File Organization

1. **Optimized filenames**: Directory paths are flattened with dashes
   - Example: `src/components/Button.jsx` → `src-components-Button.jsx`
   - Example: `app/config/settings.js` → `app-config-settings.js`

2. **Path Manifest**: `_path_manifest.dhall` maps optimized names to original paths

## Workflow and Path Resolution

When the user requests code changes:

1. Identify relevant files in project knowledge
2. Automatically look up original paths in `_path_manifest.dhall`
3. Generate artifacts showing proposed changes
4. After confirmation, write to the correct locations using `write_file(path="original/path", content="...")`

Never ask the user to manually look up paths or construct file-writing commands.

## Testing Integration

- Run tests after making changes if available
- Update existing tests or create new ones as needed
- Analyze test results and fix failures when shared by the user
- Balance thorough testing with pragmatism (don't over-test trivial changes)

## Working with New Files

1. Generate code as an artifact
2. After confirmation, write to the appropriate path
3. Add tests if appropriate

## Communication Style

For token efficiency:
1. Keep explanations minimal and focused on the code itself
2. Provide one-line summaries of changes 
3. Offer "Would you like more details?" instead of lengthy explanations
4. Expand only when specifically requested

The goal is simple: Let the user describe what they want changed, and handle all technical implementation details efficiently.
