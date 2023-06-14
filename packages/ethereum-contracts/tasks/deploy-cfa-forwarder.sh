#!/usr/bin/env bash
set -eux

# Usage:
# tasks/deploy-cfa-forwarder.sh <network> <contract-addr>
#
# Example:
# tasks/deploy-cfa-forwarder.sh optimism-goerli 0xcfa132e353cb4e398080b9700609bb008eceb125
#
# The invoking account needs to be (co-)owner of the resolver and governance
#
# important ENV vars:
# RELEASE_VERSION, DETERMINISTIC_DEPLOYER_PK, RESOLVER_ADMIN_TYPE, GOVERNANCE_ADMIN_TYPE
#
# You can use the npm package vanity-eth to get a deployer account for a given contract address:
# Example use: npx vanityeth -i cfa1 --contract
#
# Note that the value of DETERMINISTIC_DEPLOYER_PK needs to match the given contract-addr.
# The script will not check this, but fail (at contract verification) if not matching.
#
# For optimism the gas estimation doesn't work, requires setting EST_TX_COST.
# For polygon-mainnet, GAS_PRICE usually needs to be set.
#
# On some networks you may need to use override ENV vars for the deployment to succeed

network=$1
cfaFwdAddr=0xcfA132E353cB4E398080B9700609bb008eceB125

# deploy
npx truffle exec --network "$network" ops-scripts/deploy-deterministically.js : CFAv1Forwarder

# verify (give it a few seconds to pick up the code)
sleep 5
npx truffle run --network "$network" verify CFAv1Forwarder@"$cfaFwdAddr"

# set resolver
ALLOW_UPDATE=1 npx truffle exec --network "$network" ops-scripts/resolver-set-key-value.js : CFAv1Forwarder "$cfaFwdAddr"

# create gov action
npx truffle exec --network "$network" ops-scripts/gov-set-trusted-forwarder.js : 0x0000000000000000000000000000000000000000 "$cfaFwdAddr" 1

# TODO: on mainnets, the resolver entry should be set only after the gov action was signed & executed
