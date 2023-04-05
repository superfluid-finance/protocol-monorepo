#!/usr/bin/env bash

P=$(echo package.json packages/*/package.json packages/*/*/package.json)
# find duplicated devDependencies in packages
echo "$P" | xargs -- jq -r ".devDependencies | keys | .[]" | \
    sort | uniq -c | \
    grep -ve '^ *1' | awk '{print $2}' | while read -r i;do
    echo "$i"
    echo "$P" | xargs -- grep "\"$i\""
    echo
    exit 42
done
