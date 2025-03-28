#!/bin/bash
# Script to prepare and release clod to Hackage
# This script handles the release process while avoiding entering credentials through Claude

set -e  # Exit on any error

# Extract version from cabal file
VERSION=$(grep "^version:" clod.cabal | sed 's/version: *//')
echo "=== Preparing clod $VERSION for Hackage release ==="

# 1. Verify tests pass
echo "=== Running tests ==="
cabal test

# 2. Generate documentation
echo "=== Building documentation for Hackage ==="
cabal haddock --haddock-for-hackage

# 3. Create source distribution
echo "=== Creating source distribution ==="
cabal sdist

# 4. Check package
echo "=== Checking package ==="
cabal check

# 5. Test build
echo "=== Testing build ==="
cabal build --disable-documentation

# 6. Create tag
echo "=== Creating Git tag ==="
echo "Do you want to create git tag v$VERSION? [y/N]"
read -r response
if [[ "$response" =~ ^[Yy] ]]; then
  git tag -a "v$VERSION" -m "Release version $VERSION"
  
  echo "Do you want to push the tag to origin? [y/N]"
  read -r push_response
  if [[ "$push_response" =~ ^[Yy] ]]; then
    git push origin "v$VERSION"
  fi
fi

# 7. Upload to Hackage
echo "=== Ready to upload to Hackage ==="
echo "The following commands will upload the package to Hackage:"
echo
echo "  cabal upload --publish dist-newstyle/sdist/clod-$VERSION.tar.gz"
echo "  cabal upload --documentation --publish dist-newstyle/clod-$VERSION-docs.tar.gz"
echo
echo "Run these commands manually to avoid entering Hackage credentials through Claude."
echo
echo "NOTE: You'll need to have your Hackage username and password ready."
echo "For security reasons, this script does NOT run these commands automatically."

echo "=== Release preparation complete ==="
