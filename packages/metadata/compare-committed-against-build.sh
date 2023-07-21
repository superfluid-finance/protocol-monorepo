#!/bin/bash
# This script checks that the built files are consistent with the committed files.

set -xe

# Hash a file using sha256sum and cut the hash from the output
function hashFiles() {
    sha256sum $1 | cut -d ' ' -f 1
}

# Hash the commited files
main_list_committed_hash=$(hashFiles ./main/networks/list.cjs)
module_list_committed_hash=$(hashFiles ./module/networks/list.js)

# Build the files
./build.sh

# Hash the built files
main_list_built_hash=$(hashFiles './main/networks/list.cjs')
module_list_built_hash=$(hashFiles './module/networks/list.js')

# Compare the hashes and exit if they are not equal
if [ "$main_list_committed_hash" != "$main_list_built_hash" ] || [ "$module_list_committed_hash" != "$module_list_built_hash" ]; then
    echo "The built files are not consistent with the committed files. Please run ./build.sh and commit the changes."
    exit 1
fi