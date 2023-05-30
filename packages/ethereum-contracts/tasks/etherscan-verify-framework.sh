#!/usr/bin/env bash

# verification script for etherscan-like explorers.
# takes 2 arguments: the canonical network name and a file with a list of contract addresses to verify.
# tries to verify the (sub)set of contracts listed in the file.
# if proxy addresses are provided, verification against up-to-date logic contracts will only succeed
# once they point to those (after gov upgrade execution)

set -x

TRUFFLE_NETWORK=$1
ADDRESSES_VARS=$2

# network specifics
case $TRUFFLE_NETWORK in
    eth-goerli | \
    eth-sepolia | \
    polygon-mumbai | \
    optimism-goerli | \
    arbitrum-goerli | \
    avalanche-fuji )
        echo "$TRUFFLE_NETWORK is testnet"
        IS_TESTNET=1
        ;;
    eth-mainnet | \
    polygon-mainnet | \
    optimism-mainnet | \
    arbitrum-one | \
    avalanche-c | \
    bsc-mainnet | \
    celo-mainnet | \
    xdai-mainnet )
        echo "$TRUFFLE_NETWORK is mainnet"
        IS_TESTNET=
        ;;
    *)
        echo "Unknown network: $TRUFFLE_NETWORK"
        if [ -z "$SKIP_IF_UNSUPPORTED" ]; then
            exit 1;
        else
            exit 0;
        fi
esac

# shellcheck disable=SC1090
source "$ADDRESSES_VARS"

FAILED_VERIFICATIONS=()
function try_verify() {
    echo # newline for better readability
    npx truffle run --network "$TRUFFLE_NETWORK" verify "$@" ||
        FAILED_VERIFICATIONS[${#FAILED_VERIFICATIONS[@]}]="$*"
        # NOTE: append using length so that having spaces in the element is not a problem
}

function link_library() {
    local contract_name="$1"
    local library_name="$2"
    local library_address="$3"

    cp -f "build/contracts/${contract_name}.json" "build/contracts/${contract_name}.json.bak"
    jq -s '.[0] * .[1]' \
        "build/contracts/${contract_name}.json.bak" \
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
        ) > "build/contracts/${contract_name}.json"
}


if [ -n "$CONSTANT_OUTFLOW_NFT_LOGIC" ]; then
    try_verify ConstantOutflowNFT@"${CONSTANT_OUTFLOW_NFT_LOGIC}"
fi

if [ -n "$CONSTANT_INFLOW_NFT_LOGIC" ]; then
    try_verify ConstantInflowNFT"${CONSTANT_INFLOW_NFT_LOGIC}"
fi

if [ -n "$POOL_ADMIN_NFT_LOGIC" ]; then
    try_verify PoolAdminNFT@"${POOL_ADMIN_NFT_LOGIC}"
fi

if [ -n "$POOL_MEMBER_NFT_LOGIC" ]; then
    try_verify PoolMemberNFT@"${POOL_MEMBER_NFT_LOGIC}"
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
        try_verify SuperfluidGovernanceII@"${SUPERFLUID_GOVERNANCE}" --custom-proxy SuperfluidGovernanceIIProxy
    fi
fi

if [ -n "$SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC" ]; then
    try_verify SuperTokenFactory@"${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}"
fi
if [ -n "$SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY" ]; then
    try_verify SuperTokenFactory@"${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$CONSTANT_OUTFLOW_NFT_PROXY" ]; then
    try_verify ConstantOutflowNFT@"${CONSTANT_OUTFLOW_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$CONSTANT_INFLOW_NFT_PROXY" ]; then
    try_verify ConstantInflowNFT@"${CONSTANT_INFLOW_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$POOL_ADMIN_NFT_PROXY" ]; then
    try_verify PoolAdminNFT@"${POOL_ADMIN_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$POOL_MEMBER_NFT_PROXY" ]; then
    try_verify PoolMemberNFT@"${POOL_MEMBER_NFT_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$SUPERFLUID_SUPER_TOKEN_LOGIC" ]; then
    try_verify SuperToken@"${SUPERFLUID_SUPER_TOKEN_LOGIC}"
    mv -f build/contracts/SuperToken.json.bak build/contracts/SuperToken.json
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

if [ -n "$SUPERFLUID_POOL_DEPLOYER_ADDRESS" ]; then
    try_verify SuperfluidPoolDeployer@"${SUPERFLUID_POOL_DEPLOYER_ADDRESS}"
fi

if [ -n "$GDA_LOGIC" ]; then
    try_verify GeneralDistributionAgreementV1@"${GDA_LOGIC}"
fi

if [ -n "$GDA_PROXY" ]; then
    try_verify GeneralDistributionAgreementV1@"${GDA_PROXY}" --custom-proxy UUPSProxy
fi

if [ -n "$SUPERFLUID_POOL_BEACON" ]; then
    try_verify SuperfluidUpgradeableBeacon@"${SUPERFLUID_POOL_BEACON}"
fi

if [ -n "$SUPERFLUID_POOL_LOGIC" ]; then
    try_verify SuperfluidPool@"${SUPERFLUID_POOL_LOGIC}"
fi

mv -f build/contracts/InstantDistributionAgreementV1.json.bak build/contracts/InstantDistributionAgreementV1.json

if [ -n "$SUPER_TOKEN_NATIVE_COIN" ];then
    # special case: verify only the proxy
    # it is expected to point to a SuperToken logic contract which is already verified
    try_verify SETHProxy@"${SUPER_TOKEN_NATIVE_COIN}"
fi

set +x

echo "Failed verifications (may be incomplete, better visually check the log!):"
printf -- "- %s\n" "${FAILED_VERIFICATIONS[@]}"
exit ${#FAILED_VERIFICATIONS[@]}
