#!/usr/bin/env bash

# RUN: ./deploy-self-hosted-network.sh -n <network_name> -v <version_label> -i <ipfs_api_url> --node <node url>

# Define networks as an array
# TODO: Generate Superfluid self-hosted naming schema
# and use jq and get the networks from metadata.json networks file
HOSTED_NETWORKS=("polygon-zkevm-testnet" "xdai-mainnet" "polygon-mainnet")
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
            if [ -z "$2" ]; then
                echo "Error: Value for --version_label option is missing."
                exit 1
            fi
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


# Set default Subgraph node URL and IPFS API if not provided
IPFS_API=${IPFS_API:-$DEFAULT_IPFS_API}

# Deploy function
function deploy_subgraph {
    local version_label="$1"
    local network="$2"
    NODE_URL=${EXTERNAL_NODE_URL:-"https://$network.subgraph.x.superfluid.dev/admin/"}

    # Check if required arguments are provided
    if [ -z "$network" ] || [ -z "$version_label" ]; then
        echo "Please provide both --network and --version-label options."
        exit 1
    fi

    echo "********* Deploying protocol-$version_label-$network subgraph to $network network... **********"

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
    $GRAPH_CLI create "protocol-$version_label-$network" --node "$NODE_URL" && \
    $GRAPH_CLI deploy "protocol-$version_label-$network" \
        --version-label "$version_label" \
        --node "$NODE_URL" \
        --ipfs "$IPFS_API"
}


if [[ "$NETWORK" == "all" ]]; then
    for network in "${HOSTED_NETWORKS[@]}"; do
        deploy_subgraph "$VERSION_LABEL" "$network"
    done
else
    deploy_subgraph "$VERSION_LABEL" "$NETWORK"
fi
