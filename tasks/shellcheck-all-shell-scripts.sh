#!/usr/bin/env bash

set -xe

# Change this to the root directory you want to start searching from
root_dir="./"

# Find all bash files in the root directory and its subdirectories
# filter out node_modules (npm deps) and lib (foundry deps)
files=$(find "$root_dir" -type f -name "*.sh" -not -path "*/node_modules/*"  -not -path "*/lib/*")

# loop through the shell scripts and run shellcheck on it
for file in $files; do
    echo "Checking $file ..."
    shellcheck "$file"
done
