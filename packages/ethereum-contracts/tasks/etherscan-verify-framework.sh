#!/usr/bin/env bash

# verification script for etherscan-like explorers.
# takes 2 arguments: the canonical network name and a file with a list of contract addresses to verify.
# tries to verify the (sub)set of contracts listed in the file.
# if proxy addresses are provided, verification against up-to-date logic contracts will only succeed
# once they point to those (after gov upgrade execution)

set -x

CONTRACTS_DIR=build/truffle

TRUFFLE_NETWORK=$1
ADDRESSES_VARS=$2

if [ -z "$ADDRESSES_VARS" ]; then
    echo "no addresses provided, fetching myself..."
    ADDRESSES_VARS="/tmp/$TRUFFLE_NETWORK.addrs"
    npx truffle exec --network "$TRUFFLE_NETWORK" ops-scripts/info-print-contract-addresses.js : "$ADDRESSES_VARS" || exit 1
fi

# shellcheck disable=SC1090
source "$ADDRESSES_VARS"

FAILED_VERIFICATIONS=()
function try_verify() {
    echo # newline for better readability
    npx truffle run --network "$TRUFFLE_NETWORK" verify "$@" ||
        FAILED_VERIFICATIONS[${#FAILED_VERIFICATIONS[@]}]="$*"
        # NOTE: append using length so that having spaces in the element is not a problem
        # TODO: version 0.6.5 of the plugin seems to not reliably return non-zero if verification fails
}

function link_library() {
    local contract_name="$1"
    local library_name="$2"
    local library_address="$3"

    cp -f "$CONTRACTS_DIR/${contract_name}.json" "$CONTRACTS_DIR/${contract_name}.json.bak"
    jq -s '.[0] * .[1]' \
        "$CONTRACTS_DIR/${contract_name}.json.bak" \
        <(cat <<EOF
{
    "networks": {
        "$NETWORK_ID": {
            "links": {
                "${library_name}": "${library_address}"
            }
        }
    }
}
EOF
        ) > "$CONTRACTS_DIR/${contract_name}.json"
}

if [ -n "$RESOLVER" ]; then
    try_verify Resolver@"${RESOLVER}"
fi

if [ -n "$SUPERFLUID_HOST_LOGIC" ]; then
    # verify the logic contract. May or may not be already set as a proxy implementation
    try_verify Superfluid@"${SUPERFLUID_HOST_LOGIC}"
fi
if [ -n "$SUPERFLUID_HOST_PROXY" ]; then
    # by verifying against the proxy address, the contracts are "linked" in the Explorer
    try_verify Superfluid@"${SUPERFLUID_HOST_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$SUPERFLUID_GOVERNANCE" ]; then
    if [ -n "$IS_TESTNET" ];then
        try_verify TestGovernance@"${SUPERFLUID_GOVERNANCE}"
    else
        if [ -n "$SUPERFLUID_GOVERNANCE_LOGIC" ]; then
            try_verify SuperfluidGovernanceII@"${SUPERFLUID_GOVERNANCE_LOGIC}"
        fi
        try_verify SuperfluidGovernanceII@"${SUPERFLUID_GOVERNANCE}" --custom-proxy SuperfluidGovernanceIIProxy
    fi
fi

if [ -n "$SUPERFLUID_LOADER" ]; then
    try_verify SuperfluidLoader@"${SUPERFLUID_LOADER}"
fi

if [ -n "$SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC" ]; then
    try_verify SuperTokenFactory@"${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}"
fi
if [ -n "$SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY" ]; then
    try_verify SuperTokenFactory@"${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$CONSTANT_OUTFLOW_NFT_LOGIC" ]; then
    try_verify ConstantOutflowNFT@"${CONSTANT_OUTFLOW_NFT_LOGIC}"
fi

if [ -n "$CONSTANT_INFLOW_NFT_LOGIC" ]; then
    try_verify ConstantInflowNFT@"${CONSTANT_INFLOW_NFT_LOGIC}"
fi

if [ -n "$CONSTANT_OUTFLOW_NFT_PROXY" ]; then
    try_verify ConstantOutflowNFT@"${CONSTANT_OUTFLOW_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$CONSTANT_INFLOW_NFT_PROXY" ]; then
    try_verify ConstantInflowNFT@"${CONSTANT_INFLOW_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$SUPERFLUID_SUPER_TOKEN_LOGIC" ]; then
    try_verify SuperToken@"${SUPERFLUID_SUPER_TOKEN_LOGIC}"
fi

if [ -n "$CFA_LOGIC" ]; then
    try_verify ConstantFlowAgreementV1@"${CFA_LOGIC}"
fi
if [ -n "$CFA_PROXY" ]; then
    try_verify ConstantFlowAgreementV1@"${CFA_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$SLOTS_BITMAP_LIBRARY_ADDRESS" ]; then
    try_verify SlotsBitmapLibrary@"${SLOTS_BITMAP_LIBRARY_ADDRESS}"
fi

link_library "InstantDistributionAgreementV1" "SlotsBitmapLibrary" "${SLOTS_BITMAP_LIBRARY_ADDRESS}"
if [ -n "$IDA_LOGIC" ]; then
    try_verify InstantDistributionAgreementV1@"${IDA_LOGIC}"
fi
if [ -n "$IDA_PROXY" ]; then
    try_verify InstantDistributionAgreementV1@"${IDA_PROXY}" --custom-proxy UUPSProxy
fi
mv -f $CONTRACTS_DIR/InstantDistributionAgreementV1.json.bak $CONTRACTS_DIR/InstantDistributionAgreementV1.json

if [ -n "$SUPER_TOKEN_NATIVE_COIN" ];then
    # special case: verify only the proxy
    # it is expected to point to a SuperToken logic contract which is already verified
    try_verify SuperToken@"${SUPER_TOKEN_NATIVE_COIN}" --custom-proxy SETHProxy
fi

# optional peripery contracts

if [ -n "$TOGA" ];then
    try_verify TOGA@"${TOGA}"
fi

if [ -n "$BATCH_LIQUIDATOR" ];then
    try_verify BatchLiquidator@"${BATCH_LIQUIDATOR}"
fi

if [ -n "$FLOW_SCHEDULER" ];then
    try_verify FlowScheduler@"${FLOW_SCHEDULER}"
fi

if [ -n "$VESTING_SCHEDULER" ];then
    try_verify VestingScheduler@"${VESTING_SCHEDULER}"
fi

set +x

echo "Failed verifications (may be incomplete, better visually check the log!):"
printf -- "- %s\n" "${FAILED_VERIFICATIONS[@]}"
exit ${#FAILED_VERIFICATIONS[@]}
