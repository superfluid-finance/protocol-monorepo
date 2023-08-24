#!/usr/bin/env bash

# Define networks as an array
# TODO: Generate Superfluid self-hosted naming schema
# and use jq and get the networks from metadata.json networks file
HOSTED_NETWORKS=("polygon-zkevm-testnet" "polygon-mainnet")

GRAPH_CLI="npx --package=@graphprotocol/graph-cli -- graph"
IPFS_API=${EXTERNAL_IPFS_API:-$SUPERFLUID_IPFS_API}
NETWORK=""
VERSION_LABEL=""


# Usage, instructions
usage() {
    echo "Usage: $0 -n <network_name> -v <version_label>"
    exit 1
}

# Command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -n|--network)
            NETWORK="$2"
            shift 2
            ;;
        -v|--version-label)
            VERSION_LABEL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Deploy subgraph function
deploy_subgraph() {
    local version_label="$1"
    local network="$2"

    if [ -z "$network" ] || [ -z "$version_label" ]; then
        echo "Please provide both --network and --version-label options."
        usage
    fi    

    if [[ ! " ${HOSTED_NETWORKS[*]} " =~ $network ]]; then
        echo "The network, $network, is not deployed to Superfluid hosted subgraphs."
        exit 1
    fi

    echo "********* Deploying protocol-$version_label-$network subgraph to $network network... **********"
    NODE_URL="${SUBGRAPH_URL_TEMPLATE//\{\{NETWORK\}\}/$network}/admin/"

    $GRAPH_CLI create "protocol-$version_label-$network" --node "$NODE_URL" && \
    $GRAPH_CLI deploy "protocol-$version_label-$network" \
        --version-label "$version_label" \
        --node "$NODE_URL" \
        --ipfs "$IPFS_API"
}

# Main deployment logic
if [ -z "$NETWORK" ] || [ -z "$VERSION_LABEL" ]; then
    echo "Error: Both --network and --version-label options are required."
    usage
elif [ "$NETWORK" == "all" ]; then
    for network in "${HOSTED_NETWORKS[@]}"; do
        deploy_subgraph "$VERSION_LABEL" "$network"
    done
else
    deploy_subgraph "$VERSION_LABEL" "$NETWORK"
fi
