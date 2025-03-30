# Claude Code Guidelines

## Initial Reminders

⚠️ **REMINDER**: Before we proceed, please confirm you'll follow the instructions in CRITICAL.md, especially regarding not changing technical approaches without consulting me first.

## Guidelines for Claude Code

This repository is NOT read-only. You are expected to make changes, commit them, and help evolve the codebase.

The git repo for the project is in clod/ --that's where you should run commands, commit, etc. The parent directory (clod-contain/) exists for temporary files, build directories, output files, and other artifacts that shouldn't be committed to git. You should use this containing directory for outputs rather than putting temporary files inside the clod/ directory itself.

If you encounter an unexpected limitation or challenge, ask me before taking action--your ability to problem solve on your own is encouraged, but sometimes there are constraints or tools that you don't know about. Let's talk about it. When you learn something useful offer to write it back to this document (or clod/HASKELL.md) for future sessions.

I value your creativity and want you to "think outside the box" and offer novel solutions, just be sure to keep me in the loop so we can work together as effective partners.

## Project Purpose

This project is meant to be:
1. A useful tool (it cuts my Claude API spend roughly in half)
2. A showcase of human-AI collaboration

We chose Haskell for its advanced type system and expressive language features that let you reason about code that you can't run and keeps that code concise so more of it fits in your context window. The pure functional nature means that humans in the loop can focus on the uncertainty at boundaries while you take care of the purely functional work more-or-less unattended.

Given this is a showcase project, at all times you should focus on using the most idiomatic and expressive Haskell possible. Every commit may be someone's first encounter with a human/AI hybrid, so take care to always give a good first impression.

## Auto-compact *IMPORTANT*
WHEN AUTO-COMPACTING: Make your response extremely brief - DO NOT create detailed summaries. 
Simply state: "Session auto-compacted due to context limits. Please start a new session." 
Do not analyze code, create summaries, or use more tokens than absolutely necessary.

IMPORTANT: DO NOT clear the scroll buffer when auto-compacting. The user needs to scroll up and see what was happening before the auto-compact occurred.

If you have learned anything useful about working with Haskell, update HASKELL.md with that information in a format easy for future Claudes to understand.

## Newlines
All text files should have a newline at the end unless there is a specific need for one to be missing.

## Directory Structure Best Practices

- Use clod/ for all code, documentation, and git-tracked files
- Use the parent clod-contain/ directory for:
  - Test outputs
  - Example program outputs
  - Temporary build artifacts
  - Generated files that shouldn't be in version control
- Always add output directories to .gitignore 
- When implementing examples, use the parent directory structure for clean separation

## URLs and Includes

You have my permission in advance to read all helpful URLs.

Please now read the clod/HASKELL.md file to learn how we use Haskell together and then read the clod/SPEC.md file to learn how this program should work--never make changes to the clod/SPEC.md file without asking the user to confirm. If instructions differ in code comments or other documentation bring those to my attention--clod/SPEC.md has final say in all matters but I may have missed something and need to update it.