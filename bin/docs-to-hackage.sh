#!/bin/sh
set -e

# Extract version from cabal file
VERSION=$(grep "^version:" ../clod.cabal | sed 's/version: *//')

echo "Uploading documentation for clod-$VERSION"

# Must force HTTP 1.1 to get around networking issues
curl --http1.1 -X PUT --data-binary "@dist-newstyle/clod-$VERSION-docs.tar.gz" -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --user fuzz "https://hackage.haskell.org/package/clod-$VERSION/docs"
