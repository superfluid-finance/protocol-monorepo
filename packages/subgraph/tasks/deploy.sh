#!/usr/bin/env bash

JQ="npx --package=node-jq -- jq"

# shellcheck disable=SC2207
GRAPH_CLI="npx --package=@graphprotocol/graph-cli -- graph"
SUPPORTED_VENDORS=( "graph" "satsuma" "superfluid" )

# vendor specific config

# shellcheck disable=SC2034,SC2207
GRAPH_NETWORKS=( $($JQ -r .[] ./hosted-service-networks.json) ) || exit 1
# shellcheck disable=SC2034
GRAPH_NODE="https://api.thegraph.com/deploy/"
# shellcheck disable=SC2034
GRAPH_IPFS_API="https://api.thegraph.com/ipfs"
# shellcheck disable=SC2034
GRAPH_EXTRA_ARGS="--product hosted-service --access-token $THE_GRAPH_ACCESS_TOKEN"
# shellcheck disable=SC2034
GRAPH_SUBGRAPH_NAME="superfluid-finance/protocol-$RELEASE_BRANCH-$NETWORK"

# shellcheck disable=SC2034
SATSUMA_NETWORKS=( "matic" "xdai" "eth-mainnet" "eth-sepolia" "optimism-mainnet" )
# shellcheck disable=SC2034
SATSUMA_NODE="https://app.satsuma.xyz/api/subgraphs/deploy"
# shellcheck disable=SC2034
SATSUMA_IPFS_API="https://ipfs.satsuma.xyz"
# shellcheck disable=SC2034
SATSUMA_EXTRA_ARGS="--deploy-key $SATSUMA_DEPLOY_KEY"
# shellcheck disable=SC2034
SATSUMA_SUBGRAPH_NAME="protocol-$RELEASE_BRANCH"

# shellcheck disable=SC2034
SUPERFLUID_NETWORKS=( "polygon-zkevm-testnet" "polygon-mainnet" )
# SUPERFLUID_NODE is network specific and thus constructed below.
# It depends on SUBGRAPH_URL_TEMPLATE which must be provided as env var.
# shellcheck disable=SC2034
SUPERFLUID_IPFS_API="${EXTERNAL_IPFS_API:-$SUPERFLUID_IPFS_API}"
# shellcheck disable=SC2034
SUPERFLUID_EXTRA_ARGS=""
# shellcheck disable=SC2034
SUPERFLUID_SUBGRAPH_NAME="$NETWORK"

# end of vendor specific config

VENDOR=""
NETWORK=""
RELEASE_BRANCH=""
VERSION_LABEL=""

print_usage_and_exit() {
    echo "Usage: $0 -o graph|satsuma|superfluid -n <network_name> -r <release_branch> -v <version_label>"
    exit 1
}

# Command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -o|--vendor)
            VENDOR="${2}"
            shift 2
            ;;
        -n|--network)
            NETWORK="$2"
            shift 2
            ;;
        -r|--release-branch)
            RELEASE_BRANCH="$2"
            shift 2
            ;;
        -v|--version-label)
            VERSION_LABEL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            print_usage_and_exit
            ;;
    esac
done

# Expected arguments: vendor, version_label, network_name
deploy_subgraph() {
    local vendor="$1"
    local version_label="$2"
    local network="$3"

    # We can safely ignore this warning, becasue the value in network won't contain whitespaces
    # shellcheck disable=SC2199,SC2076
    if [[ ! " ${NETWORKS_REF[@]} " =~ " $network " ]]; then
        echo "The network, $network, is currently not on the list of networks supported by vendor $vendor"
        exit 1
    fi

    # create references to the config vars we need
    local -n nodeRef="${vendor^^}_NODE"
    local -n ipfsApiRef="${vendor^^}_IPFS_API"
    local -n extraArgsRef="${vendor^^}_EXTRA_ARGS"
    local -n subgraphNameRef="${vendor^^}_SUBGRAPH_NAME"
    echo "********* Deploying $network subgraph to $vendor **********"
    $GRAPH_CLI deploy \
        "${subgraphNameRef}" \
        --node "${nodeRef}" \
        --ipfs "${ipfsApiRef}" \
        --version-label "$version_label" \
        "${extraArgsRef}"
}

# Argument validation
if [ -z "$VENDOR" ] || [ -z "$NETWORK" ] || [ -z "$RELEASE_BRANCH" ] || [ -z "$VERSION_LABEL" ]; then
    print_usage_and_exit
fi

# We can safely ignore this warning, becasue the value in vendor won't contain whitespaces
# shellcheck disable=SC2199,SC2076
if [[ ! " ${SUPPORTED_VENDORS[@]} " =~ " $vendor " ]]; then
    print_usage_and_exit
fi

# create a reference to the network list we need
declare -n NETWORKS_REF="${vendor^^}_NETWORKS"

# Handle all vs specific network
if [ "$NETWORK" == "all" ]; then
    for network in "${NETWORKS_REF[@]}"; do
        # shellcheck disable=SC2034
        SUPERFLUID_NODE="${SUBGRAPH_URL_TEMPLATE//\{\{network\}\}/$network}/admin"
        deploy_subgraph "$VENDOR" "$VERSION_LABEL" "$network"
    done
else
    # shellcheck disable=SC2034
    SUPERFLUID_NODE="${SUBGRAPH_URL_TEMPLATE//\{\{NETWORK\}\}/$network}/admin"
    deploy_subgraph "$VENDOR" "$VERSION_LABEL" "$NETWORK"
fi
