#!/bin/bash

# verification script for etherscan-like explorers.
# takes 2 arguments: the canonical network name and a file with a list of contract addresses to verify.
# tries to verify the (sub)set of contracts listed in the file.
# if proxy addresses are provided, verification against up-to-date logic contracts will only succeed
# once they point to those (after gov upgrade execution)

set -x

TRUFFLE_NETWORK=$1
ADDRESSES_VARS=$2

echo TRUFFLE_NETWORK=$TRUFFLE_NETWORK
echo ADDRESSES_VARS=$ADDRESSES_VARS

# network specifics
case $TRUFFLE_NETWORK in
    eth-goerli | \
    polygon-mumbai | \
    optimism-goerli | \
    arbitrum-goerli | \
    avalanche-fuji )
        echo "$TRUFFLE_NETWORK is testnet"
        IS_TESTNET=1
        ;;
    polygon-mainnet | \
    optimism-mainnet | \
    arbitrum-one | \
    avalanche-c | \
    bsc-mainnet | \
    xdai-mainnet )
        echo "$TRUFFLE_NETWORK is mainnet"
        IS_TESTNET=
        ;;
    *)
        echo "Unknown network: $TRUFFLE_NETWORK"
        if [ -z $SKIP_IF_UNSUPPORTED ]; then
            exit 1;
        else
            exit 0;
        fi
esac

echo "Reading addresses vars..."
source "$ADDRESSES_VARS"
echo NETWORK_ID=$NETWORK_ID

FAILED_VERIFICATIONS=()
function try_verify() {
    npx truffle run --network $TRUFFLE_NETWORK verify "$@"
    # NOTE: append using length so that having spaces in the element is not a problem
    [ $? != 0 ] && FAILED_VERIFICATIONS[${#FAILED_VERIFICATIONS[@]}]="$@"
}

echo SUPERFLUID_HOST
if [ ! -z "$SUPERFLUID_HOST_LOGIC" ]; then
    # verify the logic contract. May or may not be already set as a proxy implementation
    try_verify Superfluid@${SUPERFLUID_HOST_LOGIC}
fi
if [ ! -z "$SUPERFLUID_HOST_PROXY" ]; then
    # by verifying against the proxy address, the contracts are "linked" in the Explorer
    try_verify Superfluid@${SUPERFLUID_HOST_PROXY} --custom-proxy UUPSProxy
fi

echo SUPERFLUID_GOVERNANCE
if [ ! -z "$SUPERFLUID_GOVERNANCE" ]; then
    if [ ! -z "$IS_TESTNET" ];then
        try_verify TestGovernance@${SUPERFLUID_GOVERNANCE}
    else
        try_verify SuperfluidGovernanceII@${SUPERFLUID_GOVERNANCE} --custom-proxy SuperfluidGovernanceIIProxy
    fi
fi

echo SUPERFLUID_SUPER_TOKEN_FACTORY
if [ ! -z "$SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC" ]; then
    try_verify SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}
fi
if [ ! -z "$SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY" ]; then
    try_verify SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY} --custom-proxy UUPSProxy
fi

echo SUPERFLUID_SUPER_TOKEN_LOGIC
if [ ! -z "$SUPERFLUID_SUPER_TOKEN_LOGIC" ]; then
    if [ -z "$NO_FORCE_CONSTRUCTOR_ARGS" ]; then
        # it is required to provide the constructor arguments manually, because the super token logic is created through a contract not an EOA
        SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS=$(node -e 'console.log("'${SUPERFLUID_HOST_PROXY}'".toLowerCase().slice(2).padStart(64, "0"))')
        try_verify SuperToken@${SUPERFLUID_SUPER_TOKEN_LOGIC} --forceConstructorArgs string:${SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS}
    else
        echo "!!! WARNING !!! Cannot verify super token logic due to forceConstructorArgs not supported."
    fi
fi

echo CFA
if [ ! -z "$CFA_LOGIC" ]; then
    try_verify ConstantFlowAgreementV1@${CFA_LOGIC}
fi
if [ ! -z "$CFA_PROXY" ]; then
    try_verify ConstantFlowAgreementV1@${CFA_PROXY} --custom-proxy UUPSProxy
fi

echo SlotsBitmapLibrary
if [ ! -z "$SLOTS_BITMAP_LIBRARY_ADDRESS" ]; then
    try_verify SlotsBitmapLibrary@${SLOTS_BITMAP_LIBRARY_ADDRESS}
fi

echo IDA
# NOTE: do library linking ourselves
cp -f build/contracts/InstantDistributionAgreementV1.json build/contracts/InstantDistributionAgreementV1.json.bak
jq -s '.[0] * .[1]' \
    build/contracts/InstantDistributionAgreementV1.json.bak \
    <(cat <<EOF
{
    "networks": {
        "$NETWORK_ID": {
            "links": {
                "SlotsBitmapLibrary": "$SLOTS_BITMAP_LIBRARY_ADDRESS"
            }
        }
    }
}
EOF
    ) > build/contracts/InstantDistributionAgreementV1.json
if [ ! -z "$IDA_LOGIC" ]; then
    try_verify InstantDistributionAgreementV1@${IDA_LOGIC}
fi
if [ ! -z "$IDA_PROXY" ]; then
    try_verify InstantDistributionAgreementV1@${IDA_PROXY} --custom-proxy UUPSProxy
fi
mv -f build/contracts/InstantDistributionAgreementV1.json.bak build/contracts/InstantDistributionAgreementV1.json

if [ ! -z "$SUPER_TOKEN_NATIVE_COIN" ];then
    echo SUPER_TOKEN_NATIVE_COIN
    # special case: verify only the proxy
    # it is expected to point to a SuperToken logic contract which is already verified
    try_verify SETHProxy@${SUPER_TOKEN_NATIVE_COIN}
fi

set +x

echo "Failed verifications:"
printf -- "- %s\n" "${FAILED_VERIFICATIONS[@]}"
exit ${#FAILED_VERIFICATIONS[@]}
