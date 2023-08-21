#!/usr/bin/env bash

# TODO: use jq and get the networks from metadata.json networks file
HOSTED_NETWORKS=( "matic" "xdai" "eth-mainnet" "eth-sepolia" "optimism-mainnet" )
GRAPH_CLI="npx --package=@graphprotocol/graph-cli -- graph"
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
        echo "The network, $network, is currently not on the list of networks deployed to Satsuma."
        exit 1
    fi

    echo "********* Deploying $network subgraph to Satsuma. **********"
    $GRAPH deploy "$network" \
        --version-label "$version_label" \
        --node https://app.satsuma.xyz/api/subgraphs/deploy \
        --deploy-key "$SATSUMA_DEPLOY_KEY" \
        --ipfs https://ipfs.satsuma.xyz

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
