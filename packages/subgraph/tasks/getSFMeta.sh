#!/bin/bash

echo "export let commitHash = \"${COMMIT_HASH}\";
export let configuration = \"${CONFIGURATION}\";
export let branch = \"${BRANCH}\";
export let tag = \"${TAG}\";" > src/meta.ignore.ts