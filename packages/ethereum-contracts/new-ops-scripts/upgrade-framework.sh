#!/usr/bin/env bash
#
# Framework upgrade via Foundry. When governance admin is a Safe, captures payloads for safe-ops.ts.
# SIMULATE=1 simulates without broadcast. DRY_RUN=1 is an alias for SIMULATE.
#
set -e
set -o pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
METADATA_JSON="${METADATA_JSON:-$PKG_ROOT/../metadata/networks.json}"

# shellcheck source=/dev/null
[ -f "$PKG_ROOT/.env" ] && . "$PKG_ROOT/.env"
# shellcheck source=/dev/null
[ -f "$PKG_ROOT/../.env" ] && . "$PKG_ROOT/../.env"
# shellcheck source=/dev/null
[ -f "$SCRIPT_DIR/lib/network-config.sh" ] && . "$SCRIPT_DIR/lib/network-config.sh"

NETWORK=$1
WALLET_NAME=${WALLET_NAME:-sf-ops}

if [ -z "$NETWORK" ]; then
    echo "Usage: $0 <NETWORK>"
    exit 1
fi

if ! jq -e --arg n "$NETWORK" 'any(.[]; .name == $n)' "$METADATA_JSON" >/dev/null 2>&1; then
    echo "Network $NETWORK not found in networks.json" >&2
    exit 1
fi

PROVIDER_URL=$(get_rpc_url "$NETWORK") || exit 1
echo "Using RPC: $PROVIDER_URL"
echo "Using WALLET: $WALLET_NAME"

HOST_ADDRESS=$(get_host "$NETWORK")
RESOLVER_ADDRESS=$(jq -r '.[] | select(.name == "'"$NETWORK"'") | .contractsV1.resolver' "$METADATA_JSON")

if [[ -z "$HOST_ADDRESS" || "$HOST_ADDRESS" == "null" || ! "$HOST_ADDRESS" =~ ^0x[a-fA-F0-9]{40}$ ]]; then
    echo "HOST_ADDRESS is not a valid Ethereum address" >&2
    exit 1
fi
if [[ -z "$RESOLVER_ADDRESS" || "$RESOLVER_ADDRESS" == "null" || ! "$RESOLVER_ADDRESS" =~ ^0x[a-fA-F0-9]{40}$ ]]; then
    echo "RESOLVER_ADDRESS is not a valid Ethereum address" >&2
    exit 1
fi

echo "Using HOST_ADDRESS: $HOST_ADDRESS"
echo "Using RESOLVER_ADDRESS: $RESOLVER_ADDRESS"

if [ -z "${VERSION_STRING:-}" ]; then
    PACKAGE_VERSION=$(jq -r '.version' "$PKG_ROOT/package.json")
    GIT_REVISION=$(git -C "$PKG_ROOT/../.." rev-parse --short=16 HEAD)
    VERSION_STRING="${PACKAGE_VERSION}-${GIT_REVISION}"
fi
echo "Using VERSION_STRING: $VERSION_STRING"

export HOST_ADDRESS
export RESOLVER_ADDRESS
export VERSION_STRING
export RELEASE_VERSION="${RELEASE_VERSION:-v1}"

cd "$PKG_ROOT"

forge_args=(script foundry-scripts/UpgradeFramework.s.sol:UpgradeFramework --rpc-url "$PROVIDER_URL")
if [[ -n "${SIMULATE:-}" || -n "${DRY_RUN:-}" ]]; then
    forge_args+=(--private-key 0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80)
else
    forge_args+=(--account "$WALLET_NAME" --broadcast --verify)
    if [[ -n "${ETHERSCAN_API_KEY:-}" ]]; then
        echo "ETHERSCAN_API_KEY is set"
        forge_args+=(--etherscan-api-key "$ETHERSCAN_API_KEY")
    else
        echo "ETHERSCAN_API_KEY is not set"
    fi
fi

SAFE_PAYLOADS_DIR="$PKG_ROOT/.tmp/safe-payloads"
mkdir -p "$SAFE_PAYLOADS_DIR"
SAFE_PAYLOADS_FILE=$(mktemp "$SAFE_PAYLOADS_DIR/payloads.XXXXXX.jsonl")
trap 'rm -f "$SAFE_PAYLOADS_FILE"' EXIT
export SAFE_PAYLOADS_FILE

if ! with_keystore_password forge "${forge_args[@]}"; then
    echo "Forge script failed"
    exit 1
fi

safe_ops_args=(propose-file --rpc-url "$PROVIDER_URL" --payload-file "$SAFE_PAYLOADS_FILE" --mode single)
if [[ -n "${SIMULATE:-}" || -n "${DRY_RUN:-}" ]]; then
    safe_ops_args+=(--dry-run)
fi
if [[ -n "${SAFE_ORIGIN:-}" ]]; then
    safe_ops_args+=(--origin "$SAFE_ORIGIN")
fi

npx ts-node "$SCRIPT_DIR/safe-ops.ts" "${safe_ops_args[@]}"
