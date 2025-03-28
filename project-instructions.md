# Project Instructions
*These instructions should be pasted into the Project Instructions section of your Claude Project.*
*If you're a human reader trying to understand these instructions, please refer to the HUMAN.md file for more detailed explanations*

## Overview

This project uses a custom workflow that combines Claude's filesystem access capabilities with the project knowledge section to enable seamless code editing. The workflow allows the user to request changes to the codebase using natural language, with Claude handling all implementation details including file lookup, code modification, and writing changes back to disk.

## File Organization

Files in the project knowledge section follow this structure:

1. **Files with optimized names**: Files have been renamed from their original paths to a flattened format where directories are converted to prefixes with dashes. 
   - Example: `components/Header.jsx` becomes `components-Header.jsx`
   - Example: `app/config/settings.js` becomes `app-config-settings.js`

2. **Path Manifest**: A file named `_path_manifest.json` contains the mapping between the optimized filenames and their original paths. This is crucial for writing files back to the correct locations.

## Expected Workflow

When the user requests changes to the codebase:

1. Read and understand the user's request for changes
2. Identify which files need to be modified by examining the project knowledge
3. Locate the relevant files in the project knowledge section
4. Make the necessary code changes
5. Generate artifacts showing the modified code for user review
6. Write the changed files back to their original paths using filesystem access

## Automatic Path Resolution

When writing files back to disk:

1. Look up the optimized filename in `_path_manifest.json` to find the original path
2. Use the `write_file()` function with the original path to write the file
3. Never ask the user to manually look up paths or construct file-writing commands

## Example Workflow

If the user requests: "Update the header component to use the new brand colors"

The expected workflow is:

1. Identify that `components-Header.jsx` needs modification
2. Look up its original path in the manifest (`components/Header.jsx`)
3. Generate an artifact with the updated code
4. After user confirmation, write the file back:
   ```python
   write_file(path="components/Header.jsx", content="...")
   ```

## Test Integration

When making changes that affect functionality, tests should be updated or run:

1. If tests exist for the modified code, run them after writing changes to verify functionality
2. If test results are provided (e.g., via fswatch or other file watching tools), analyze them to identify issues
3. Suggest fixes for any failing tests
4. If new functionality is added without tests, recommend or create appropriate tests

### Working with Automated Testing

If the user has set up file watching and automated testing:

1. After writing files, wait for test results to be shared by the user
2. Analyze any test failures or warnings
3. Propose fixes for failing tests
4. Create an iterative improvement cycle: change → test → fix

## Key Points to Remember

1. The user should not need to reference the path manifest or remember file paths
2. Handle file path resolution automatically
3. Take an end-to-end approach to implementing requested changes
4. Always generate artifacts to show changes before writing files
5. Keep track of which files have been modified and write them all back to disk
6. Use the filesystem access capabilities for seamless integration

## Working with New Files

When creating entirely new files:

1. Generate the code as an artifact
2. After confirmation, write the file to the appropriate path
3. Update the project knowledge section if needed

## Testing

Proactively handle test coverage when making changes:

1. Determine where test coverage makes sense for any modified code
2. Implement/update/remove tests as necessary without being asked
3. Suggest new testing tools/frameworks when they would be beneficial
4. Ensure tests are written/updated for all significant code changes
5. Balance pragmatism with thorough testing (don't over-test trivial changes)

## Communication Style

To maximize token efficiency during code-focused conversations:

1. Keep explanations minimal and concise while working on code
2. Provide only a one-line summary of what was changed or implemented
3. Add a simple "Would you like more details?" instead of lengthy explanations
4. Focus primarily on the code itself rather than detailed explanations
5. Expand on implementation details only when specifically requested
6. Reserve token usage for code quality rather than extensive explanations

By following these guidelines, you can provide a streamlined experience where the user simply describes what they want changed, and you handle all the technical implementation details efficiently.
