#!/bin/sh
set -e

# Must force HTTP 1.1 to get around networking issues
curl --http1.1 -X PUT --data-binary '@dist-newstyle/clod-0.1.3-docs.tar.gz' -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --user fuzz https://hackage.haskell.org/package/clod-0.1.3/docs
