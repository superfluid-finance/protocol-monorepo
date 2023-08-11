#!/usr/bin/env sh

list_linked_libraries() {
    jq -r '.bytecode.linkReferences | map(keys) | .[][]'
}

find build/foundry/out/ -name '*.json' | while read -r i;do
    cat < "$i" | list_linked_libraries
done | sort | uniq
