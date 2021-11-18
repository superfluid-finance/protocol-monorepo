#!/bin/bash
TAG="$(git describe --abbrev=0 --tag)"
BRANCH="$(git branch --show-current)"

cat > src/meta.ignore.ts << EOF
export let commitHash = ${COMMIT_HASH};
export let configuration = ${CONFIGURATION};
export let branch = ${BRANCH};
export let tag = ${TAG};
EOF