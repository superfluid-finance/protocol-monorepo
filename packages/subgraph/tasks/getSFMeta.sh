#!/usr/bin/env bash
BRANCH="$(git branch --show-current)"

cat > src/meta.ignore.ts << EOF
export let commitHash = "${COMMIT_HASH}";
export let configuration = "${CONFIGURATION}";
export let branch = "${BRANCH}";
EOF