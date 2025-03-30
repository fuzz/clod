#\!/bin/bash

find test/ -name "*.hs" -type f -exec sed -i '' 's/databaseFile = .*,/databaseFile = tmpDir <\/> ".clod" <\/> "database.dhall",/g' {} \;
find test/ -name "*.hs" -type f -exec sed -i '' 's/previousStaging = Nothing,/previousStaging = Nothing,/g' {} \;
find test/ -name "*.hs" -type f -exec sed -i '' 's/flushMode = False,/flushMode = False,/g' {} \;
find test/ -name "*.hs" -type f -exec sed -i '' 's/lastMode = False,/lastMode = False,/g' {} \;

# Remove the commas at the beginning of lines
find test/ -name "*.hs" -type f -exec sed -i '' 's/^\([ \t]*\),/\1/g' {} \;

# Verify and list all files with configuration problems
echo "Checking for remaining issues..."
grep -rn "," --include="*.hs" test/ | grep -B 1 "timestamp"
