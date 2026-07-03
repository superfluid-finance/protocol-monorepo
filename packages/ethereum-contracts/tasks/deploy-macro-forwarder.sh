#!/usr/bin/env bash
set -eu
set -o pipefail

# Usage:
# tasks/deploy-macro-forwarder.sh <network>
#
# Prefer new-ops-scripts/deploy-clearmacro-forwarder.sh for ClearMacroForwarderV1.

# shellcheck source=/dev/null
source .env

set -x

network=$1
expectedContractAddr="0xFD0268E33111565dE546af2675351A4b1587F89F"
deployerPk=$MACROFWD_DEPLOYER_PK

export DETERMINISTIC_DEPLOYER_PK=$deployerPk
export EXPECTED_ADDRESS=$expectedContractAddr

tmpfile=$(mktemp)
./new-ops-scripts/deploy-deterministic-forwarder.sh "$network" BlindMacroForwarder | tee "$tmpfile"
contractAddr=$(tail -n 1 "$tmpfile")
rm "$tmpfile"

echo "deployed to $contractAddr"
if [[ $contractAddr != "$expectedContractAddr" ]]; then
    echo "contract address not as expected!"
    if [ -z "$SKIP_ADDRESS_CHECK" ]; then
        exit 1
    fi
fi

sleep 5
./new-ops-scripts/verify-forwarder.sh "$network" BlindMacroForwarder "$contractAddr" || true

ALLOW_UPDATE=1 ./new-ops-scripts/register-forwarder.sh "$network" BlindMacroForwarder "$contractAddr"
./new-ops-scripts/activate-forwarder.sh "$network" "$contractAddr"
