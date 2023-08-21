#!/usr/bin/env bash

JQ="npx --package=node-jq -- jq"

# shellcheck disable=SC2207
HOSTED_NETWORKS=( $($JQ -r .[] ./hosted-service-networks.json) ) || exit 1
GRAPH_CLI="npx --package=@graphprotocol/graph-cli -- graph"
NETWORK=""
VERSION_LABEL=""


# Usage, instructions
usage() {
    echo "Usage: $0 -n <network_name> -v <version_label> [--vendor <vendor_name>]"
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

    echo "********* Deploying $network subgraph to The Graph. **********"
    # $GRAPH deploy \
    #     --product hosted-service \
    #     superfluid-finance/protocol-"$1"-"$2" \
    #     --node https://api.thegraph.com/deploy/ \
    #     --ipfs https://api.thegraph.com/ipfs \
    #     --access-token "$THE_GRAPH_ACCESS_TOKEN"

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
