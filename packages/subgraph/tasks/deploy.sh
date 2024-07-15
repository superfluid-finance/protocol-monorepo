#!/usr/bin/env bash

set -eux

JQ="jq"

# shellcheck disable=SC2207
GRAPH_CLI="npx --package=@graphprotocol/graph-cli --yes -- graph"
# shellcheck disable=SC2207
GOLDSKY_CLI="npx --package=@goldskycom/cli --yes -- goldsky"
SUPPORTED_VENDORS=( "graph" "satsuma" "superfluid" "goldsky" "airstack" )

# list of supported networks by vendor

# shellcheck disable=SC2034,SC2207
GRAPH_NETWORKS=( "polygon-mainnet" "eth-mainnet" )
# shellcheck disable=SC2034
SATSUMA_NETWORKS=( "polygon-mainnet" "xdai-mainnet" "eth-mainnet" "eth-sepolia" "optimism-mainnet" "base-mainnet")
# shellcheck disable=SC2034
SUPERFLUID_NETWORKS=( "polygon-mainnet" "xdai-mainnet" "base-mainnet" "optimism-mainnet" "arbitrum-one" "celo-mainnet" "bsc-mainnet" "avalanche-c" "optimism-sepolia" "scroll-sepolia" "scroll-mainnet" "degenchain" "base-sepolia")
# shellcheck disable=SC2034
GOLDSKY_NETWORKS=( "polygon-mainnet" "xdai-mainnet" "eth-mainnet" "base-mainnet" "optimism-mainnet" "arbitrum-one" "bsc-mainnet" "avalanche-c" "optimism-sepolia" "scroll-sepolia" "scroll-mainnet" "eth-sepolia" "avalanche-fuji" "base-sepolia")
# shellcheck disable=SC2034
AIRSTACK_NETWORKS=( "degenchain")

declare -A VENDOR_NETWORKS=(
    ["graph"]="${GRAPH_NETWORKS[@]}"
    ["satsuma"]="${SATSUMA_NETWORKS[@]}"
    ["superfluid"]="${SUPERFLUID_NETWORKS[@]}"
    ["goldsky"]="${GOLDSKY_NETWORKS[@]}"
    ["airstack"]="${AIRSTACK_NETWORKS[@]}"
)

VENDOR=""
NETWORK=""
DEPLOYMENT_ENV=""
VERSION_LABEL=""

print_usage_and_exit() {
    echo "Usage: $0 -o graph|satsuma|superfluid|goldsky|airstack -n <network_name> -r <deployment_env> -v <version_label>"
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
        -r|--deployment-env)
            DEPLOYMENT_ENV="$2"
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

deploy_to_graph() {
    local network="$1"

    # name mapping for subgraphs created before introducing canonical names
    local -A legacyNetworkNames=(
        ["xdai-mainnet"]="xdai"
        ["polygon-mainnet"]="matic"
    )

    local graphNetwork="${legacyNetworkNames[$network]:-$network}"
    local subgraphName="protocol-$DEPLOYMENT_ENV-$graphNetwork"

    echo "********* Deploying $network subgraph $subgraphName to The Graph (hosted service). **********"

    $GRAPH_CLI deploy \
        --studio "$subgraphName" \
        --deploy-key "$THE_GRAPH_ACCESS_TOKEN" \
        --version-label "$VERSION_LABEL"
}

deploy_to_satsuma() {
    local network="$1"

    # name mapping for subgraphs (no excuse here for not having canonical names)
    local -A legacyNetworkNames=(
        ["xdai-mainnet"]="xdai"
        ["polygon-mainnet"]="matic"
    )

    local satsumaNetwork="${legacyNetworkNames[$network]:-$network}"

    echo "********* Deploying $network subgraph to Satsuma. **********"
    $GRAPH_CLI deploy "$satsumaNetwork" \
        --version-label "$VERSION_LABEL" \
        --node https://subgraphs.alchemy.com/api/subgraphs/deploy \
        --deploy-key "$SATSUMA_DEPLOY_KEY" \
        --ipfs https://ipfs.satsuma.xyz
}

deploy_to_superfluid() {
    local network="$1"

    echo "********* Deploying $network subgraph to Superfluid (self hosted). **********"
    local nodeUrl="${SUBGRAPH_URL_TEMPLATE//\{\{NETWORK\}\}/$network}/admin/"
    local subgraphName="protocol-$DEPLOYMENT_ENV"
    echo "node url: $nodeUrl, subgraph name: $subgraphName"

    $GRAPH_CLI create "$subgraphName" --node "$nodeUrl"
    $GRAPH_CLI deploy "$subgraphName" \
        --version-label "$VERSION_LABEL" \
        --node "$nodeUrl" \
        --ipfs "$SUPERFLUID_IPFS_API"
}

deploy_to_goldsky() {
    local network="$1"
    # TODO: use tagging?

    #Get subgraph version from package.json
    PACKAGE_JSON_PATH="package.json"
    SUBGRAPH_VERSION=$($JQ -r '.version' $PACKAGE_JSON_PATH)

    local subgraphName="protocol-$DEPLOYMENT_ENV-$network/$SUBGRAPH_VERSION"

    # Note: when using Graph CLI to deploy, it implicitly triggers build too, but Goldsky CLI doesn't, so we do it explicitly.
    $GRAPH_CLI build

    echo "********* Deploying $network subgraph $subgraphName to Goldsky. **********"
    $GOLDSKY_CLI subgraph deploy \
        "$subgraphName" \
        --path . \
        --token "$GOLDSKY_API_KEY"
}

deploy_to_airstack() {
    local network="$1"
    local nodeUrl="https://subgraph.airstack.xyz/indexer/"
    local subgraphName="protocol-$DEPLOYMENT_ENV-$network"

    echo "********* Deploying $network subgraph $subgraphName to Airstack. **********"
    $GRAPH_CLI create "$subgraphName" --node "$nodeUrl" --access-token "$AIRSTACK_API_KEY"
    $GRAPH_CLI deploy \
        --version-label "$VERSION_LABEL" \
        --node "$nodeUrl" \
        --deploy-key "$AIRSTACK_API_KEY" \
        --ipfs https://ipfs.airstack.xyz/ipfs/api/v0 \
        --headers '{"Authorization": "'"$AIRSTACK_API_KEY"'"}' \
        "$subgraphName"
}

# Vendor specific function dispatcher
# Expected arguments:
# $1 - vendor
# $2 - canonical network name
deploy_to() {
    local vendor="$1"
    local network="$2"

    # check if network is supported by vendor
    local -n networksRef="${vendor^^}_NETWORKS"
    # We can safely ignore this warning, becasue the value in network won't contain whitespaces
    # shellcheck disable=SC2199,SC2076
    if [[ ! " ${networksRef[@]} " =~ " $network " ]]; then
        echo "The network, $network, is currently not on the list of networks supported by $vendor."
        exit 1
    fi

    npx ts-node ./scripts/buildNetworkConfig.ts "$network" "$vendor"

    # prepare the manifest prior to deployment
    # this generates the subgraph.yaml and
    # inputs the correct addresses for the specified network ($2)
    ./tasks/prepare-manifest.sh "$network"

    case "$vendor" in
    graph)
        deploy_to_graph "$network"
        ;;
    satsuma)
        deploy_to_satsuma "$network"
        ;;
    superfluid)
        deploy_to_superfluid "$network"
        ;;
    goldsky)
        deploy_to_goldsky "$network"
        ;;
    airstack)
        deploy_to_airstack "$network"
        ;;
    *)
        print_usage_and_exit
        ;;
    esac
}

# Argument validation
if [ -z "$VENDOR" ] || [ -z "$NETWORK" ] || [ -z "$DEPLOYMENT_ENV" ] || [ -z "$VERSION_LABEL" ]; then
    print_usage_and_exit
fi

# We can safely ignore this warning, because the value in vendor won't contain whitespaces
# shellcheck disable=SC2199,SC2076
if [[ ! " ${SUPPORTED_VENDORS[@]} " =~ " $VENDOR " ]]; then
    print_usage_and_exit
fi

# Handle all vs specific network
if [ "$NETWORK" == "all" ]; then
    for network in ${VENDOR_NETWORKS[$VENDOR]}; do
        deploy_to "$VENDOR" "$network"
    done
else
    deploy_to "$VENDOR" "$NETWORK"
fi
