#!/usr/bin/env bash

JQ="npx --package=node-jq -- jq"

# make sure that if any step fails, the script fails
set -xe

if [ "$SUBGRAPH_RELEASE_TAG" == "feature" ];then
    # we only support matic and goerli feature endpoints
    # however, we don't want to be blocked by matic for feature
    NETWORKS=("goerli")
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
        ["eth-goerli"]="goerli"
        ["polygon-mumbai"]="mumbai"
    )

    GRAPH_NETWORK="${LEGACY_NETWORK_NAMES[$i]:-$i}"

    SUBGRAPH_ENDPOINT=https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-$SUBGRAPH_RELEASE_TAG-$GRAPH_NETWORK

    testSchemaAndQueries

done
