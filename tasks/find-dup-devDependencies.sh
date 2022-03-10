#!/bin/sh

P=$(echo packages/*/package.json package.json)
# find duplicated devDependencies in packages
jq -r '.devDependencies | keys | .[]' $P | \
    sort | uniq -c | \
    grep -ve '^ *1' | awk '{print $2}' | while read i;do
    echo $i
    grep '"'$i'"' $P
    echo
done
