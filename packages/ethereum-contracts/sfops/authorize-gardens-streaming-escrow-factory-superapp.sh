#!/usr/bin/env bash
#
# Grant ACL_SUPERAPP_REGISTRATION_ROLE to the 1Hive Gardens StreamingEscrowFactory proxy on each
# chain where it is deployed, so the factory (or its deployer context) can register Super Apps via
# the SimpleACL path on the host (see Superfluid.sol _enforceAppRegistrationPermissioning).
#
# Addresses are the ENVS.STREAMING_ESCROW_FACTORY proxy values from:
#   https://github.com/1Hive/gardens-v2/blob/main/pkg/contracts/config/networks.json
#
# Invokes: new-ops-scripts/acl-grant-superapp-registration.sh (cast send / cast call, Foundry wallet).
#
# Previous Truffle governance version (setAppRegistrationKey): kept as
#   authorize-gardens-streaming-escrow-factory-superapp.truffle-gov-backup.sh
#
# Usage:
#   ./authorize-gardens-streaming-escrow-factory-superapp.sh                 # all chains below
#   ./authorize-gardens-streaming-escrow-factory-superapp.sh eth-mainnet   # single network
#
# Env (optional):
#   WALLET_NAME   — Foundry keystore account for cast send (default: sf-ops), same as gov-action.sh
#   SIMULATE=1    — same as other new-ops-scripts / acl-grant-superapp-registration.sh: `cast call`
#                   only (no broadcast, no keystore unlock for send). Omit for real `cast send`.
#   METADATA_JSON, PROVIDER_URL_OVERRIDE, PROVIDER_URL_TEMPLATE — see new-ops-scripts/lib/network-config.sh
#
# Prerequisites: run from packages/ethereum-contracts (or cwd is set below). Foundry `cast` on PATH.
#
set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PKG_ROOT"

# shellcheck source=/dev/null
[ -f "$PKG_ROOT/.env" ] && . "$PKG_ROOT/.env"
# shellcheck source=/dev/null
[ -f "$PKG_ROOT/../.env" ] && . "$PKG_ROOT/../.env"

ACL_SCRIPT="new-ops-scripts/acl-grant-superapp-registration.sh"

# Metadata network name (matches truffle-config.js network keys and networks.json `name`) and
# factory proxy (Gardens networks.json ENVS.STREAMING_ESCROW_FACTORY).
read -r -d '' CHAIN_TABLE <<'EOF' || true
eth-mainnet 0x87ad877e190a18fa4da4f4cabfd5c137353f5f57
optimism-mainnet 0x71520d667f5ab45f68c4e8455ffa1f9a7f792eac
xdai-mainnet 0x7472b2197b38233a97d0134d62ccd38aa4b93112
polygon-mainnet 0x856a184c5547a0945f66d0583d4c223813be1651
base-mainnet 0x3b3333665de09494c2491171c06a6a161f319032
arbitrum-one 0xbe52a852eaf824cd221aa55c3ee054f6c8c712df
celo-mainnet 0xa2e7637a6ffd7576b7352ec7a34b9ee38e7e7e6c
EOF

run_one() {
    local network_name=$1
    local factory_proxy=$2

    echo "======== $network_name — factory proxy $factory_proxy ========"
    if [[ "${SIMULATE:-}" == "1" ]]; then
        SIMULATE=1 "$ACL_SCRIPT" "$network_name" "$factory_proxy"
    else
        "$ACL_SCRIPT" "$network_name" "$factory_proxy"
    fi
}

main() {
    if [[ ! -f "$ACL_SCRIPT" ]]; then
        echo "Missing $PKG_ROOT/$ACL_SCRIPT" >&2
        exit 1
    fi

    local selection
    if [[ -n "${1:-}" ]]; then
        selection=$(echo "$CHAIN_TABLE" | grep -E "^$1 " || true)
        if [[ -z "$selection" ]]; then
            echo "Unknown network: $1 (not in this script's table)" >&2
            exit 1
        fi
    else
        selection=$CHAIN_TABLE
    fi

    while read -r network_name factory_proxy; do
        [[ -z "$network_name" ]] && continue
        run_one "$network_name" "$factory_proxy"
    done <<<"$selection"
}

main "$@"
