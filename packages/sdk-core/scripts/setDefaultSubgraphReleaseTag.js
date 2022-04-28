const { writeFileSync } = require('fs')
const { join } = require('path')

// This solution is necessary to embed environment variable data at build time.
// Example: GitHub Actions runs canary build and wants to embed "dev" Subgraph release tag as the default.

const FILE_PATH = join(__dirname, './../src/defaultSubgraphReleaseTag.json');

writeFileSync(FILE_PATH, JSON.stringify({
    value: process.env.SUBGRAPH_RELEASE_TAG || "v1"
}, null, 2)); 