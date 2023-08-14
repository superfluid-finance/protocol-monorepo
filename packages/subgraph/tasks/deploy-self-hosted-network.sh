#!/usr/bin/env bash

# RUN: ./deploy-self-hosted-network.sh -n <network_name> -v <version_label> -i <ipfs_api_url> --node <node url>

# Define networks as an array
# TODO: Generate Superfluid self-hosted naming schema
# and use jq and get the networks from metadata.json networks file
HOSTED_NETWORKS=("polygon-zkevm-testnet" "xdai-mainnet")
GRAPH_CLI="npx --package=@graphprotocol/graph-cli -- graph"
DEFAULT_IPFS_API="https://ipfs-api.x.superfluid.dev"

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
        -i|--ipfs-api)
            IPFS_API="$2"
            shift 2
            ;;
        --node)
            NODE_URL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check if required arguments are provided
if [ -z "$NETWORK" ] || [ -z "$VERSION_LABEL" ]; then
    echo "Please provide both --network and --version-label options."
    exit 1
fi

# Set default Subgraph node URL and IPFS API if not provided
IPFS_API=${IPFS_API:-$DEFAULT_IPFS_API}

# Deploy function
function deploy_subgraph {
    local version_label="$1"
    local network="$2"
    NODE_URL=${NODE_URL:-"https://$2.subgraph.x.superfluid.dev/admin/"}

    echo "Deploying subgraph $version_label to $network network..."

    echo "Version label is: $version_label"
    echo "Network is: $network"
    echo "IPFS_API is: $IPFS_API"
    echo "Subgraph Node URL: $NODE_URL"
    

    # Check if the provided network is in the array
    if [[ ! " ${HOSTED_NETWORKS[@]} " =~ " $network " ]]; then
        echo "The network, $network ,is currently not deployed to Superfluid hosted subgraphs."
        exit 1
    fi

    # Create and deploy the subgraph
    $GRAPH_CLI create protocol-v1 --node "https://$network.subgraph.x.superfluid.dev" && \
    $GRAPH_CLI deploy "$network" \
        --version-label "$version_label" \
        --node "$NODE_URL" \
        --ipfs "$IPFS_API"
}


function deploy_all_networks {
    for network in "${HOSTED_NETWORKS[@]}"; do
        deploy_subgraph "$1" "$network"
    done
}


if [[ "$NETWORK" == "all" ]]; then
    deploy_all_networks "$VERSION_LABEL"
else
    deploy_subgraph "$VERSION_LABEL" "$NETWORK"
fi