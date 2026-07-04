#!/usr/bin/env bash
#
# Read-only: for every Gardens StreamingEscrowFactory proxy in the table below, check whether
# SimpleACL.hasRole(ACL_SUPERAPP_REGISTRATION_ROLE, factory) is true on that network.
#
# Factory addresses match ENVS.STREAMING_ESCROW_FACTORY in:
#   https://github.com/1Hive/gardens-v2/blob/main/pkg/contracts/config/networks.json
#
# Usage (no arguments):
#   ./check-gardens-streaming-escrow-factory-acl.sh
#
# Exit code 0 — all checks passed (hasRole true everywhere).
# Exit code 1 — at least one failure (no role, bad RPC, missing metadata, etc.).
#
# Env (optional):
#   METADATA_JSON, RPC_URL, PROVIDER_URL_OVERRIDE, PROVIDER_URL_TEMPLATE — same as other sfops scripts
#
# Prerequisites: packages/ethereum-contracts cwd below; `cast` and `jq` on PATH.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PKG_ROOT"

METADATA_JSON="${METADATA_JSON:-$PKG_ROOT/../metadata/networks.json}"

# shellcheck source=/dev/null
[ -f "$PKG_ROOT/.env" ] && . "$PKG_ROOT/.env"
# shellcheck source=/dev/null
[ -f "$PKG_ROOT/../.env" ] && . "$PKG_ROOT/../.env"

# keccak256("ACL_SUPERAPP_REGISTRATION_ROLE") — Superfluid.sol
ACL_SUPERAPP_REGISTRATION_ROLE="0x1fd2cd0659bdcac914c39b66359256350b866e92047951635b57d928f32d9e84"

read -r -d '' CHAIN_TABLE <<'EOF' || true
eth-mainnet 0x87ad877e190a18fa4da4f4cabfd5c137353f5f57
optimism-mainnet 0x71520d667f5ab45f68c4e8455ffa1f9a7f792eac
xdai-mainnet 0x7472b2197b38233a97d0134d62ccd38aa4b93112
polygon-mainnet 0x856a184c5547a0945f66d0583d4c223813be1651
base-mainnet 0x3b3333665de09494c2491171c06a6a161f319032
arbitrum-one 0xbe52a852eaf824cd221aa55c3ee054f6c8c712df
celo-mainnet 0xa2e7637a6ffd7576b7352ec7a34b9ee38e7e7e6c
EOF

resolve_rpc() {
    local network=$1
    if [[ -n "${RPC_URL:-}" ]]; then
        echo "$RPC_URL"
        return 0
    fi
    if [[ -n "${PROVIDER_URL_OVERRIDE:-}" ]]; then
        echo "$PROVIDER_URL_OVERRIDE"
        return 0
    fi
    if [[ -n "${PROVIDER_URL_TEMPLATE:-}" ]]; then
        if [[ "$PROVIDER_URL_TEMPLATE" != *"{{NETWORK}}"* ]]; then
            echo "resolve_rpc: PROVIDER_URL_TEMPLATE must contain {{NETWORK}}" >&2
            return 1
        fi
        echo "${PROVIDER_URL_TEMPLATE//\{\{NETWORK\}\}/$network}"
        return 0
    fi
    local rpc
    rpc=$(jq -r --arg n "$network" '.[] | select(.name == $n) | .publicRPCs[0]' "$METADATA_JSON")
    if [[ -z "$rpc" || "$rpc" == "null" ]]; then
        echo "resolve_rpc: no public RPC in metadata for $network" >&2
        return 1
    fi
    echo "$rpc"
}

resolve_host() {
    local network=$1
    jq -r --arg n "$network" '.[] | select(.name == $n) | .contractsV1.host' "$METADATA_JSON"
}

parse_bool_result() {
    local out=$1
    local out_trim out_lc
    out_trim=$(echo "$out" | tr -d '[:space:]')
    out_lc=$(echo "$out_trim" | tr '[:upper:]' '[:lower:]')
    if [[ "$out_lc" == "true" ]] || [[ "$out_lc" == *"true"* ]] || [[ "$out_trim" == "0x0000000000000000000000000000000000000000000000000000000000000001" ]]; then
        return 0
    fi
    return 1
}

# Prints one status line; returns 0 if hasRole true, 1 otherwise.
check_row() {
    local network_name=$1
    local factory_proxy=$2
    local rpc host acl out

    if ! rpc=$(resolve_rpc "$network_name"); then
        echo "$network_name  factory=$factory_proxy  ERROR=no_rpc"
        return 1
    fi
    host=$(resolve_host "$network_name")
    if [[ -z "$host" || "$host" == "null" ]]; then
        echo "$network_name  factory=$factory_proxy  ERROR=no_host_in_metadata"
        return 1
    fi

    if ! acl=$(cast call "$host" "getSimpleACL()(address)" --rpc-url "$rpc" 2>/dev/null); then
        echo "$network_name  factory=$factory_proxy  ERROR=getSimpleACL_failed"
        return 1
    fi
    acl=$(echo "$acl" | tr -d '[:space:]')

    if ! out=$(cast call "$acl" "hasRole(bytes32,address)(bool)" "$ACL_SUPERAPP_REGISTRATION_ROLE" "$factory_proxy" --rpc-url "$rpc" 2>/dev/null); then
        echo "$network_name  factory=$factory_proxy  SimpleACL=$acl  ERROR=hasRole_call_failed"
        return 1
    fi

    if parse_bool_result "$out"; then
        echo "$network_name  factory=$factory_proxy  hasRole=true  SimpleACL=$acl"
        return 0
    fi
    echo "$network_name  factory=$factory_proxy  hasRole=false  SimpleACL=$acl  raw=$out"
    return 1
}

main() {
    if [[ ! -f "$METADATA_JSON" ]]; then
        echo "Missing METADATA_JSON: $METADATA_JSON" >&2
        exit 1
    fi
    if ! command -v jq >/dev/null 2>&1; then
        echo "jq is required" >&2
        exit 1
    fi
    if ! command -v cast >/dev/null 2>&1; then
        echo "cast is required" >&2
        exit 1
    fi

    if [[ -n "${1:-}" ]]; then
        echo "This script takes no arguments (checks all networks in its table)." >&2
        exit 1
    fi

    local failed=0
    echo "Checking ACL_SUPERAPP_REGISTRATION_ROLE for Gardens StreamingEscrowFactory proxies"
    echo "METADATA_JSON=$METADATA_JSON"
    echo ""

    while read -r network_name factory_proxy; do
        [[ -z "$network_name" ]] && continue
        if ! check_row "$network_name" "$factory_proxy"; then
            failed=1
        fi
    done <<<"$CHAIN_TABLE"

    echo ""
    if [[ "$failed" -eq 0 ]]; then
        echo "Summary: all rows have hasRole=true."
        exit 0
    fi
    echo "Summary: one or more rows failed (see above)." >&2
    exit 1
}

main "$@"
