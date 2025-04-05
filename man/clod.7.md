% CLOD(7) Clod 0.1.29
% Fuzz Leonard & Claude <ink@fuzz.ink>
% March 2025

# NAME

clod - project instructions and safeguards for Claude AI integration

# DESCRIPTION

This man page contains guidance on how to structure project instructions for Claude AI
and implement safeguards when using clod with Claude AI's Project Knowledge feature.
# PROJECT INSTRUCTIONS
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

2. **Path Manifest**: A file named `_path_manifest.dhall` contains the mapping between the optimized filenames and their original paths. This is crucial for writing files back to the correct locations. The file uses the Dhall configuration language.

## Expected Workflow

When the user requests changes to the codebase through Claude AI:

1. Read and understand the user's request for changes
2. Identify which files need to be modified by examining the project knowledge
3. Locate the relevant files in the project knowledge section
4. Make the necessary code changes
5. Generate artifacts showing the modified code for user review
6. Write the changed files back to their original paths using filesystem access

## Automatic Path Resolution

When writing files back to disk:

1. Look up the optimized filename in `_path_manifest.dhall` to find the original path
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

# SAFEGUARDS
# Safety Guardrails for clod 

This document outlines recommended safety practices when using clod. Since different projects have different requirements, we've provided a menu of guardrails that you can implement based on your specific needs.

## How to Implement Guardrails

To apply guardrails to your clod workflow:

1. Review this document and select appropriate guardrails for your project
2. Copy the guardrail sections you want to use
3. Add them to the **bottom of the Project Instructions section** in your Claude Project
4. This ensures the guardrails apply to all conversations in that project

For example, you might add:
```
## Project Guardrails

When working with this codebase, you should:
1. Create backups of files before modifying them
2. Only modify files that were in the original project upload
3. Get explicit confirmation for significant architectural changes
```

By placing guardrails in the Project Instructions, they become part of Claude's understanding of the project and apply to every conversation.

## Understanding Claude Features

The guardrails in this document rely on two key Claude features:

**Project Knowledge** is a feature in Claude that allows you to upload files for reference throughout your conversation without consuming your context window.

**Filesystem Access** is Claude's ability to read from and write to files on your local system (currently available only on macOS and Windows desktop applications).

## API Cost Management

Without proper controls, AI-assisted development can potentially lead to unexpected API costs. Consider implementing these guardrails:

### Token Usage Monitoring
```
When implementing changes, I should first:
1. Estimate token usage for the planned implementation
2. Inform the user of this estimate before proceeding
3. Get explicit confirmation for implementations likely to use >10K tokens
```

### Session Budgeting
```
I should track approximate token usage during this session and alert the user when approaching:
- 50% of budget (warning)
- 80% of budget (caution)
- 95% of budget (final warning)

The user's specified token budget for this session is [USER_SPECIFIED_AMOUNT].
```

### Operation Batching
```
When multiple files require changes, I should:
1. Group related changes into batches
2. Summarize all planned changes before implementation
3. Implement changes in order of dependency to minimize redundant operations
```

## Cloud Computing Cost Management

AI-assisted development involving cloud services can lead to unexpected costs that can quickly escalate from hundreds to thousands of dollars. Consider implementing these guardrails to help prevent common cost overruns seen in the wild:

### Resource Creation & Termination

```
When implementing code that provisions cloud resources, I should:
1. Verify every resource creation includes a corresponding termination mechanism
2. Suggest time-based auto-shutdown for development resources
3. Ensure EC2/VM instances have explicit termination conditions
4. Never create resources without explicit resource limits
5. Require confirmation before creating any resources with usage-based billing
6. Default to development/testing tiers unless production is explicitly requested
```

### Budget Constraints & Cost Estimation

```
For all cloud-related implementations, I should:
1. Check for and suggest adding explicit spending caps/quotas where available
2. Recommend CloudWatch/Monitoring alerts at 50% and 80% of budget
3. Include code comments highlighting potential cost implications
4. Suggest usage of spot instances/preemptible VMs when appropriate
5. Provide a rough cost estimate for any created resources
6. Include both idle/baseline costs and potential usage-based costs
7. Highlight any operations with potentially unbounded costs
```

### API Usage Protection & Optimization

```
When implementing code that calls external APIs, I should:
1. Identify opportunities for request batching to reduce API calls
2. Suggest appropriate caching strategies for repeated calls
3. Recommend rate-limiting to prevent accidental API abuse
4. Identify and warn about potential recursive or unbounded API calls
5. Always include rate limiting and usage caps
6. Implement exponential backoff for retries
7. Add monitoring/alerting for unusual usage patterns
```

### Scaling & Instance Management

```
For auto-scaling implementations, I should:
1. Ensure both scale-up AND scale-down conditions are clearly defined
2. Recommend absolute maximum instance counts regardless of load
3. Suggest gradual scaling with cooldown periods between scaling events
4. Include circuit-breaker patterns for abnormal scaling conditions
5. Always include auto-shutdown for non-production resources
6. Set appropriate instance size limits (no auto-scaling to largest instances)
7. Prefer spot/preemptible instances for batch workloads
8. Include resource tagging for cost tracking
```

### Data Transfer Awareness

```
When working with data transfer operations, I should:
1. Highlight cross-region data transfers and suggest alternatives
2. Calculate and display estimated costs for large data movements
3. Suggest compression or sampling strategies to reduce transfer volume
4. Recommend using CDNs or regional replication instead of frequent transfers
5. Consider region co-location for frequently communicating services
6. Implement incremental data processing where possible
```

### Resource Cleanup

```
For any cloud resource creation, I should:
1. Include cleanup scripts/instructions
2. Implement auto-expiry where supported
3. Use infrastructure-as-code tools to track all created resources
4. Remind users to destroy test/development resources when finished
```

### Common Cost Pitfalls

```
I should proactively warn about these common cloud cost issues:
1. Runaway autoscaling due to misconfigured metrics
2. Infinite loops or recursion in code calling paid APIs
3. Uncompressed or redundant data storage
4. Cross-region data transfer
5. Orphaned or unused resources (load balancers, volumes, etc.)
6. Development resources left running outside of working hours
7. Missing scale-down conditions in auto-scaling groups
8. Unbounded API call patterns without rate limiting
```

### Real-world Examples

```
I'm aware of common cloud cost incidents like:
1. Recursive API calls creating exponential cost growth
2. Auto-scaling without upper bounds responding to traffic spikes
3. Debugging logs accidentally set to highest verbosity in production
4. Large data exports triggered unintentionally
5. Development clusters left running over holidays/weekends
6. Cross-region replication creating ongoing transfer costs

I should proactively identify when code patterns might lead to similar incidents.
```

## Filesystem Protection

Safeguarding your filesystem during AI-assisted development is critical. Consider these protective measures:

### Safe Directories
```
I should only write to files within these safe directories:
- [LIST_OF_SAFE_DIRECTORIES]

If a requested change would modify files outside these directories, I must:
1. Alert the user to this fact
2. Require explicit confirmation before proceeding
```

### Automatic Backups
```
Before modifying any file, I should:
1. Create a backup with the suffix .bak-[TIMESTAMP]
2. Inform the user of the backup location
3. Include instructions for restoring from backup if needed
```

### Version Control Integration
```
Before implementing changes, I should:
1. Confirm the project directory is under version control
2. Advise the user to commit current changes
3. Suggest creating a branch for experimental changes
```

## Code Quality Safeguards

Maintain code quality during AI-assisted development with these guardrails:

### Test Coverage Requirements
```
When modifying code, I should:
1. Identify existing tests for the modified functionality
2. Create or update tests to maintain [DESIRED_PERCENTAGE]% test coverage
3. Run tests before marking changes as complete
```

### Static Analysis Integration
```
After making changes but before finalizing, I should:
1. Recommend running appropriate linters/static analyzers
2. If available, interpret and summarize any linting errors
3. Offer to fix common issues automatically
```

### Documentation Updates
```
For any substantive code changes, I should:
1. Update associated documentation
2. Add/update comments explaining complex logic
3. Update any affected API documentation
```

## User Education

Help users understand the capabilities and limitations of AI-assisted development:

### Progressive Disclosure
```
When working with new users, I should:
1. Focus on simple, well-defined tasks initially
2. Explain my reasoning process clearly
3. Suggest more complex operations only after successful simple operations
```

### Capability Boundaries
```
I should clearly communicate when requests fall outside my capabilities:
1. Code that requires specialized domain knowledge
2. Operations that would violate security boundaries
3. Tasks that would be more efficiently done by the human
```

### Human Review Reminders
```
After implementing changes, I should:
1. Remind the user to review all changes before deployment
2. Highlight areas that particularly warrant human review
3. Suggest specific validation steps appropriate to the changes
```

## Example Guardrail Combinations

Below are some example combinations of guardrails for common use cases:

### Basic Safety Package
```
## Project Guardrails

Before modifying any file, I should:
1. Create a backup with the suffix .bak-[TIMESTAMP]
2. Only write to files that were in the original project upload
3. Highlight any changes that might have significant architectural impacts
4. Remind you to review changes before deploying to production
```

### Developer-Focused Package
```
## Project Guardrails

When working with this codebase, I should:
1. Identify and maintain test coverage for all modified code
2. Group related changes into logical batches for review
3. Track approximate token usage during our session
4. Proactively suggest documentation updates when necessary
5. Create backups before modifying files
```

### Cloud Infrastructure Package
```
## Project Guardrails

When working with cloud infrastructure code, I should:
1. Always include resource limits and termination conditions
2. Suggest cost optimization techniques for expensive resources
3. Warn about potentially unbounded API calls or scaling conditions
4. Include cleanup scripts for any created resources
5. Default to development/testing tiers unless production is explicitly requested
```

## Implementing Guardrails

### For Individual Developers

1. Review this document and select appropriate guardrails
2. Copy relevant guardrail sections to add to your Project Instructions
3. Customize values (e.g., budget limits, safe directories) as needed

### For Teams

1. Create a standardized set of guardrails appropriate for your organization
2. Add these to your team's source control as `clod-guardrails.md`
3. Include this file in your clod uploads
4. Copy the standardized guardrails to your Project Instructions

## Best Practices

- Start with more restrictive guardrails and relax them as you gain experience
- Periodically review and update guardrails based on your experiences
- Share effective guardrails with the community by contributing to this document
- Remember that guardrails are guidelines, not absolute protection against all risks

## Contributing

If you develop effective guardrails for specific use cases, please consider contributing them back to the clod project so others can benefit from your experience.
