#!/usr/bin/env bash
#
# Authorize the 1Hive Gardens StreamingEscrowFactory proxy as a Super App deployer on each chain
# where it is deployed (registration key "k1" by default — override with REGISTRATION_KEY).
#
# Addresses are the ENVS.STREAMING_ESCROW_FACTORY proxy values from:
#   https://github.com/1Hive/gardens-v2/blob/main/pkg/contracts/config/networks.json
#
# Invokes: ops-scripts/gov-authorize-app-deployer.js (Truffle governance action).
#
# Usage:
#   ./authorize-gardens-streaming-escrow-factory-superapp.sh              # all chains below
#   ./authorize-gardens-streaming-escrow-factory-superapp.sh eth-mainnet  # single Truffle network
#
# Env (optional):
#   REGISTRATION_KEY   — passed through to the JS script (default k1 in gov-authorize-app-deployer.js)
#   EXPIRATION_TS      — Unix seconds; if unset, the script uses effectively no expiry (2^64-1)
#   DRY_RUN=1          — print commands only
#
# Prerequisites: run from repo with Truffle deps (packages/ethereum-contracts). Uses the same
# wallet / env vars as other truffle exec gov scripts (e.g. MNEMONIC or private key per truffle-config).
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

GOV_SCRIPT="ops-scripts/gov-authorize-app-deployer.js"

# Truffle network name (truffle-config.js) and factory proxy (Gardens networks.json).
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
    local truffle_net=$1
    local factory_proxy=$2
    local cmd=(npx truffle exec --network "$truffle_net" "$GOV_SCRIPT" : "$factory_proxy")
    if [[ -n "${EXPIRATION_TS:-}" ]]; then
        cmd+=("$EXPIRATION_TS")
    fi

    echo "======== $truffle_net — factory proxy $factory_proxy ========"
    if [[ "${DRY_RUN:-}" == "1" ]]; then
        printf '%q ' "${cmd[@]}"
        echo
        return 0
    fi
    "${cmd[@]}"
}

main() {
    if [[ ! -f "$GOV_SCRIPT" ]]; then
        echo "Missing $PKG_ROOT/$GOV_SCRIPT" >&2
        exit 1
    fi

    local selection
    if [[ -n "${1:-}" ]]; then
        selection=$(echo "$CHAIN_TABLE" | grep -E "^$1 " || true)
        if [[ -z "$selection" ]]; then
            echo "Unknown Truffle network: $1 (not in this script's table)" >&2
            exit 1
        fi
    else
        selection=$CHAIN_TABLE
    fi

    while read -r truffle_net factory_proxy; do
        [[ -z "$truffle_net" ]] && continue
        run_one "$truffle_net" "$factory_proxy"
    done <<<"$selection"
}

main "$@"
