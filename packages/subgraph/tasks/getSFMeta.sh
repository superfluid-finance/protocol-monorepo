#!/bin/bash
BRANCH="$(git branch --show-current)"

cat > src/meta.ignore.ts << EOF
import { Bytes } from "@graphprotocol/graph-ts";
export let commitHash = Bytes.fromHexString("${COMMIT_HASH}");
export let configuration = "${CONFIGURATION}";
export let branch = "${BRANCH}";
EOF