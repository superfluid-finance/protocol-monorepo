#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"

# make sure that if any step fails, the script fails
set -xe

if [ "$SUBGRAPH_RELEASE_TAG" == "feature" ];then
    # we only support matic and goerli feature endpoints
    # however, we don't want to be blocked by matic for feature
    NETWORKS=("goerli")
fi

if [ "$SUBGRAPH_RELEASE_TAG" == "dev" ] || [ "$SUBGRAPH_RELEASE_TAG" == "v1" ];then
    NETWORKS=( $($JQ -r .[] ../subgraph/networks.json) )
fi

function testSchemaAndQueries() {
    # generate schema.graphql with desired endpoint
    npx get-graphql-schema $SUBGRAPH_ENDPOINT > src/subgraph/schema.graphql
    # attempt to generate types with schema.graphql generated in previous step and .graphql files in entities folders
    yarn generate:graphql-types
    # attempt to run queries with desired endpoint
    export SUBGRAPH_ENDPOINT=$SUBGRAPH_ENDPOINT && npx hardhat test previous-versions-testing/runQueryTests.ts
}

# for sdk-core releases: test deployed subgraphs
for i in "${NETWORKS[@]}";do
    SUBGRAPH_ENDPOINT=https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-$SUBGRAPH_RELEASE_TAG-$i

    testSchemaAndQueries

done