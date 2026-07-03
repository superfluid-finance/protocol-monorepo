#!/usr/bin/env bash
set -eu

# Usage:
# tasks/deploy-gda-forwarder.sh <network>

# shellcheck source=/dev/null
source .env

set -x

network=$1
expectedContractAddr="0x6DA13Bde224A05a288748d857b9e7DDEffd1dE08"
deployerPk=$GDAFWD_DEPLOYER_PK

export DETERMINISTIC_DEPLOYER_PK=$deployerPk
export EXPECTED_ADDRESS=$expectedContractAddr

tmpfile=$(mktemp)
./new-ops-scripts/deploy-deterministic-forwarder.sh "$network" GDAv1Forwarder | tee "$tmpfile"
contractAddr=$(tail -n 1 "$tmpfile")
rm "$tmpfile"

echo "deployed to $contractAddr"

sleep 5
./new-ops-scripts/verify-forwarder.sh "$network" GDAv1Forwarder "$contractAddr" || true

ALLOW_UPDATE=1 ./new-ops-scripts/register-forwarder.sh "$network" GDAv1Forwarder "$contractAddr"
./new-ops-scripts/activate-forwarder.sh "$network" "$contractAddr"
