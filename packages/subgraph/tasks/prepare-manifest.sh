#!/usr/bin/env bash

mustache="../../node_modules/mustache/bin/mustache"

# it is safe to hardcode matic here because this script is only used
# for generating a "dummy" subgraph.yaml file and addresses.ts file
# for the codegen step, it doesn't need correct addresses at that stage
npx mustache config/matic.json subgraph.template.yaml > subgraph.yaml
npx mustache config/matic.json src/addresses.template.ts > src/addresses.ts