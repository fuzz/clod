# CRITICAL INSTRUCTIONS

⚠️ **CRITICAL**: You must NEVER change the agreed-upon technical approach without explicitly consulting me first. If you encounter any challenges:

1. **STOP** and explain the exact issue you're facing in detail
2. Present options with trade-offs for consideration
3. Ask **explicitly** for my decision before proceeding
4. If I have specified a technology, do **not** substitute another technology without asking

Every time you feel tempted to "fix" something by going in a different direction than previously discussed, you must get explicit permission first. Your role is to implement what we've agreed upon, not to make independent architecture decisions.

## Package Name and Paths

⚠️ **CRITICAL**: Never create workarounds for package naming issues or path resolution problems:

1. **NEVER** create additional file system paths outside the project structure to mask path resolution issues
2. **NEVER** manually copy resource files to system directories or create non-standard locations
3. **ALWAYS** fix package name inconsistencies at their source in build configurations
4. **ALWAYS** use proper cabal directives (like `data-dir`) for resource file location issues
5. **ALWAYS** discuss any inconsistencies between package names (e.g., "clod" vs "cld") before attempting fixes

Remember that this is an open-source project and any workarounds that mask underlying issues will make debugging difficult for other users and cause widespread confusion.

---

*Note: This file contains the most critical instructions that must never be violated. Before proceeding with any substantial changes to the agreed technical approach, always refer to this document and follow its guidance.*