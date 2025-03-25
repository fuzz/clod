# Contributing to Clod

Thank you for considering contributing to Clod! This document outlines the process for contributing to the project.

## Code of Conduct

This project and everyone participating in it is governed by our Code of Conduct. By participating, you are expected to uphold this code.

## How Can I Contribute?

### Reporting Bugs

- Check if the bug has already been reported
- Use the bug report template if available
- Include as much detail as possible
- Include steps to reproduce the issue
- Include the version of Clod you're using
- Include your OS and environment details

### Suggesting Enhancements

- Check if the enhancement has already been suggested
- Provide a clear description of the enhancement
- Explain why this enhancement would be useful
- Consider how the enhancement fits with the project's goals

### Pull Requests

1. Fork the repository
2. Create a branch for your changes
3. Make your changes
4. Add or update tests as necessary
5. Update documentation if needed
6. Run the test suite: `cabal test`
7. Ensure your changes meet the project's code style
8. Submit the pull request

## Development Setup

Ensure you have the following installed:

- GHC (Glasgow Haskell Compiler) 9.0 or newer
- Cabal 3.0 or newer
- Git

Clone the repository:

```bash
git clone https://github.com/fuzz/clod.git
cd clod
```

Build the project:

```bash
cabal build
```

Run the tests:

```bash
cabal test
```

## Style Guidelines

This project follows idiomatic Haskell style:

- Use 2 spaces for indentation (no tabs)
- Provide Haddock documentation for public functions
- Use meaningful variable names
- Follow the [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

## Commit Messages

- Use the present tense ("Add feature" not "Added feature")
- Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
- Reference issues and pull requests where appropriate
- Consider starting with a tag:
  - `feat:` for new features
  - `fix:` for bug fixes
  - `docs:` for documentation changes
  - `test:` for test changes
  - `refactor:` for code refactoring

## Testing

- Add tests for every new feature
- Update tests for bug fixes
- Run the test suite before submitting a pull request
- Aim for high test coverage

## Documentation

- Update the README.md if necessary
- Add Haddock documentation for new functions
- Update example code if needed
- Consider updating the CHANGELOG.md for significant changes

## Questions?

If you have any questions about contributing, feel free to open an issue or reach out to the maintainers directly.
