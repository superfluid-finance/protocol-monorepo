#!/usr/bin/env bash
#
# Withdraw accumulated yield surplus on Base and Ethereum (one Safe proposal per network).
# Wraps new-ops-scripts/batch-withdraw-yield-surplus.sh.
#
# Usage (from packages/ethereum-contracts):
#   ./sfops/batch-withdraw-yield-surplus.sh
#   SIMULATE=1 ./sfops/batch-withdraw-yield-surplus.sh
#
# Env: loaded from packages/ethereum-contracts/.env (SAFE_PROPOSER_PK, SAFE_API_KEY, etc.)
#
set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PKG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OPS_SCRIPT="$PKG_ROOT/new-ops-scripts/batch-withdraw-yield-surplus.sh"

# shellcheck source=/dev/null
[ -f "$PKG_ROOT/.env" ] && . "$PKG_ROOT/.env"
# shellcheck source=/dev/null
[ -f "$PKG_ROOT/../.env" ] && . "$PKG_ROOT/../.env"
export SAFE_PROPOSER_PK="${SAFE_PROPOSER_PK:-}"
export SAFE_API_KEY="${SAFE_API_KEY:-}"
export SAFE_TX_SERVICE_URL="${SAFE_TX_SERVICE_URL:-}"

BASE_TOKENS=(
  0xD04383398dD2426297da660F9CCA3d439AF9ce1b
  0x46fd5cfB4c12D87acD3a13e92BAa53240C661D93
)

ETH_TOKENS=(
  0x1BA8603DA702602A8657980e825A6DAa03Dee93a
  0xc22bea0be9872d8b7b3933cec70ece4d53a900da
)

run_network() {
  local network=$1
  shift
  local -a tokens=("$@")
  echo ""
  echo "======== $network (${#tokens[@]} SuperToken(s)) ========"
  "$OPS_SCRIPT" "$network" "${tokens[@]}"
}

cd "$PKG_ROOT"
run_network base-mainnet "${BASE_TOKENS[@]}"
run_network eth-mainnet "${ETH_TOKENS[@]}"

echo ""
echo "Done."
