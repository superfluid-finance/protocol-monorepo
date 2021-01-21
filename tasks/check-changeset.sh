#!/bin/bash

SOURCE_REF=$1
BASE_REF=$2

# fetch the latest commit of the base ref
git fetch --depth=1 origin refs/heads/${BASE_REF}:refs/remotes/origin/${BASE_REF}
# compare the source branch with the dev branch
git diff --name-only ${SOURCE_REF} refs/remotes/origin/${BASE_REF} > changed-files.list
echo Changed files:
echo ---
cat changed-files.list
echo ---
