# TEST SAFETY PROPOSAL

**IMPORTANT: These are proposed ideas and workflows only. They should be ignored when writing code, tests, or documentation for the current implementation.**

## The Problem

When developing software collaboratively between humans and AI assistants, critical functionality can be unintentionally lost during refactoring or implementation changes. This can happen even with test coverage in place, as demonstrated by the incident where the automatic creation of `.clodignore` files was removed without detection.

## Proposed Workflow

### 1. Separate Repositories for Code and Tests

Maintain implementation code and tests in separate repositories with different access controls:

- **Implementation Repository**: AI has read-write access to implementation code
- **Test Repository**: AI can read and run tests but not modify them without explicit human review

### 2. Test Modification Process

When tests need updating:

1. AI generates a detailed modification plan explaining why each test change is needed
2. Human reviews the plan, checking for:
   - Removal of tests for still-needed behaviors
   - Subtle changes to expected behaviors
   - Compatibility with the project specification
3. After approval, a separate AI instance with limited context receives specific instructions to update only the approved tests
4. Tests are run to verify the updates maintain coverage

### 3. Enhanced Test Documentation

#### Test Categorization

Tag tests to indicate their importance for core behaviors:

```haskell
-- @behavior-critical: Ensures .clodignore files are auto-created when missing
it "creates a default .clodignore file when one doesn't exist" $ do
  ...
```

#### Test Provenance Comments

Add brief headers explaining the purpose of critical tests:

```haskell
-- This test verifies the fundamental behavior of creating default configuration
-- files when they don't exist, which is essential for first-time users.
-- DO NOT REMOVE without explicit approval.
it "creates a default .clodignore file when one doesn't exist" $ do
  ...
```

### 4. Implementation-Test Mapping

Create a document that explicitly links key implementation parts to their corresponding tests:

```
| Functionality               | Implementation File      | Test File                     |
|-----------------------------|-----------------------------|------------------------------|
| Default file creation       | src/Clod/IgnorePatterns.hs  | test/Clod/IgnorePatternsSpec.hs |
| ...                         | ...                         | ...                         |
```

### 5. Change Review Automation

When tests are modified, generate a structured report highlighting:

- Tests that were removed
- Tests that had their assertions changed
- New tests added
- Implementation functions no longer covered by tests

## Alternative: Capability-Based Test Protection

As an alternative to separate repositories, implement capability-based access control within the same repository:

1. AI has read-write access to implementation code
2. AI has read-only access to test code by default
3. AI can request explicit permission to modify specific tests, which requires human approval
4. AI can run tests but not modify the test results

## Benefits

This approach would:

1. Preserve critical tests that document expected behaviors
2. Make test changes explicit and subject to human review
3. Maintain institutional knowledge about why certain tests exist
4. Reduce the risk of functionality regression during refactoring

## Next Steps

To implement this workflow:

1. Define clear capability boundaries for test access
2. Create templates for test modification requests
3. Document critical behaviors in a central specification
4. Develop a testing policy that requires human review for test removals