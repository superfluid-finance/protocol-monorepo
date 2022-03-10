#!/bin/sh

# find duplicated devDependencies in packages
jq -r '.devDependencies | keys | .[]' packages/*/package.json | \
    sort | uniq -c | \
    grep -ve '^ *1' | awk '{print $2}' | while read i;do
    echo $i
    grep '"'$i'"' packages/*/package.json
    echo
done
