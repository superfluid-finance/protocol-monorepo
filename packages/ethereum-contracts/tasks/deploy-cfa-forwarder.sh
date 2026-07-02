#!/usr/bin/env bash
set -eu

# Usage:
# tasks/deploy-cfa-forwarder.sh <network>
#
# See new-ops-scripts/deploy-deterministic-forwarder.sh, register-forwarder.sh, activate-forwarder.sh

# shellcheck source=/dev/null
source .env

set -x

network=$1
expectedContractAddr="0xcfA132E353cB4E398080B9700609bb008eceB125"
deployerPk=$CFAFWD_DEPLOYER_PK

export DETERMINISTIC_DEPLOYER_PK=$deployerPk
export EXPECTED_ADDRESS=$expectedContractAddr

tmpfile=$(mktemp)
./new-ops-scripts/deploy-deterministic-forwarder.sh "$network" CFAv1Forwarder | tee "$tmpfile"
contractAddr=$(tail -n 1 "$tmpfile")
rm "$tmpfile"

echo "deployed to $contractAddr"
if [[ $contractAddr != "$expectedContractAddr" ]]; then
    echo "oh no!"
    exit 1
fi

sleep 5
./new-ops-scripts/verify-forwarder.sh "$network" CFAv1Forwarder "$contractAddr" || true

ALLOW_UPDATE=1 ./new-ops-scripts/register-forwarder.sh "$network" CFAv1Forwarder "$contractAddr"
./new-ops-scripts/activate-forwarder.sh "$network" "$contractAddr"
