# Release Process for Clod

This document describes the steps to release a new version of Clod.

## Prerequisites

Before releasing, ensure you have:
- [Cabal](https://www.haskell.org/cabal/) installed
- A [Hackage](https://hackage.haskell.org/) account
- The `cabal-install` tool
- [Stack](https://docs.haskellstack.org/en/stable/README/) (optional, for Stackage releases)

## Release Checklist

1. **Update Version Numbers**
   - Update version in `clod.cabal`
   - Update version in `src/Clod/Core.hs` (version output)
   - Ensure version follows [Semantic Versioning](https://semver.org/)

2. **Update Changelog**
   - Add a new section to `CHANGELOG.md` with the new version
   - Document all notable changes since the last release
   - Categorize changes as Added, Changed, Deprecated, Removed, Fixed, or Security

3. **Run Tests**
   ```
   cabal test
   ```

4. **Build Documentation**
   ```
   cabal haddock --haddock-for-hackage
   ```

5. **Create Distribution Package**
   ```
   cabal sdist
   ```

6. **Check Package**
   ```
   cabal check
   ```

7. **Test Package Installation**
   ```
   cabal v2-build --disable-documentation
   ```

8. **Create Release Commit**
   ```
   git add .
   git commit -m "Release version X.Y.Z"
   ```

9. **Create Git Tag**
   ```
   git tag -a vX.Y.Z -m "Release version X.Y.Z"
   git push origin vX.Y.Z
   ```

10. **Upload to Hackage**
    ```
    # Option 1: Use the release script
    ./bin/release-to-hackage.sh
    
    # Option 2: Upload manually
    cabal upload --publish dist-newstyle/sdist/clod-X.Y.Z.tar.gz
    cabal upload --documentation --publish dist-newstyle/clod-X.Y.Z-docs.tar.gz
    ```

11. **Prepare for Next Development Cycle**
   - Update version in `clod.cabal` to next development version (X.Y.Z-dev)
   - Commit changes
   ```
   git add clod.cabal
   git commit -m "Prepare for next development cycle"
   ```

## Stackage Integration

To get the package included in Stackage:

1. Fork the [commercialhaskell/stackage](https://github.com/commercialhaskell/stackage) repository
2. Add the package to `build-constraints.yaml`
3. Create a pull request

## GitHub Release

After releasing to Hackage:

1. Go to [GitHub Releases](https://github.com/fuzz/clod/releases)
2. Create a new release based on the tag
3. Add release notes from the CHANGELOG
4. Attach the binary distributions if available

## Troubleshooting

- If the Hackage upload fails, check the validation errors and fix issues
- If documentation doesn't render correctly, ensure all modules are properly documented
- If the check fails, make sure the package meets all Hackage requirements
