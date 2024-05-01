#!/usr/bin/env bash

JQ="npx --package=node-jq -- jq"

# make sure that if any step fails, the script fails
set -xe

if [ "$SUBGRAPH_RELEASE_TAG" == "feature" ];then
    NETWORKS=("matic")
fi

if [ "$SUBGRAPH_RELEASE_TAG" == "dev" ] || [ "$SUBGRAPH_RELEASE_TAG" == "v1" ];then
    # shellcheck disable=SC2207
    NETWORKS=( $($JQ -r .[] ../subgraph/hosted-service-networks.json) )
fi

function testSchemaAndQueries() {
    # generate schema.graphql with desired endpoint
    npx get-graphql-schema "$SUBGRAPH_ENDPOINT" > src/subgraph/schema.graphql
    # attempt to generate types with schema.graphql generated in previous step and .graphql files in entities folders
    yarn generate:graphql-types
    # attempt to run queries with desired endpoint
    export SUBGRAPH_ENDPOINT=$SUBGRAPH_ENDPOINT && npx hardhat test previous-versions-testing/queryTests.ts
}

# for sdk-core releases: test deployed subgraphs
for i in "${NETWORKS[@]}";do
    # name mapping for subgraphs created before introducing canonical names
    declare -A LEGACY_NETWORK_NAMES=(
        ["xdai-mainnet"]="xdai"
        ["polygon-mainnet"]="matic"
    )

    GRAPH_NETWORK="${LEGACY_NETWORK_NAMES[$i]:-$i}"

    if [ "$SUBGRAPH_RELEASE_TAG" == "v1" ]; then
        # No need for the legacy name here
        SUBGRAPH_ENDPOINT="https://${NETWORKS[$i]}.subgraph.x.superfluid.dev"
    else
        SUBGRAPH_ENDPOINT="https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-$SUBGRAPH_RELEASE_TAG-$GRAPH_NETWORK"
    fi

    testSchemaAndQueries

done
