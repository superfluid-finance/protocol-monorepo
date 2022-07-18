#!/bin/bash

set -x

TRUFFLE_NETWORK=$1
ADDRESSES_VARS=$2

echo TRUFFLE_NETWORK=$TRUFFLE_NETWORK
echo ADDRESSES_VARS=$ADDRESSES_VARS

# network specifics
case $TRUFFLE_NETWORK in
    eth-goerli | eth-rinkeby | eth-ropsten | eth-kovan | \
    polygon-mumbai | \
    optimism-kovan | \
    arbitrum-rinkeby | \
    avalanche-fuji )
        echo "$TRUFFLE_NETWORK is testnet"
        IS_TESTNET=1
        ;;
    polygon-mainnet | \
    optimism-mainnet | \
    arbitrum-one | \
    avalanche-c | \
    bsc-mainnet )
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
try_verify UUPSProxy@${SUPERFLUID_HOST_PROXY} --implementation Superfluid

echo SUPERFLUID_GOVERNANCE
if [ ! -z "$IS_TESTNET" ];then
try_verify TestGovernance@${SUPERFLUID_GOVERNANCE}
else
try_verify SuperfluidGovernanceIIProxy@${SUPERFLUID_GOVERNANCE} --implementation SuperfluidGovernanceII
fi

echo SUPERFLUID_SUPER_TOKEN_FACTORY
try_verify UUPSProxy@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY} --implementation SuperTokenFactory

echo SUPERFLUID_SUPER_TOKEN_LOGIC
if [ -z "$NO_FORCE_CONSTRUCTOR_ARGS" ];then
    # it is required to provide the constructor arguments manually, because the super token logic is created through a contract not an EOA
    SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS=$(node -e 'console.log("'${SUPERFLUID_HOST_PROXY}'".toLowerCase().slice(2).padStart(64, "0"))')
    try_verify UUPSProxy@${SUPERFLUID_SUPER_TOKEN_LOGIC} --implementation SuperToken --forceConstructorArgs string:${SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS}
else
    echo "!!! WARNING !!! Cannot verify super token logic due to forceConstructorArgs not supported."
fi

echo CFA
try_verify UUPSProxy@${CFA_PROXY} --implementation ConstantFlowAgreementV1

echo SlotsBitmapLibrary
try_verify SlotsBitmapLibrary@${SLOTS_BITMAP_LIBRARY_ADDRESS}

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
try_verify UUPSProxy@${IDA_PROXY} --implementation InstantDistributionAgreementV1
mv -f build/contracts/InstantDistributionAgreementV1.json.bak build/contracts/InstantDistributionAgreementV1.json

if [ ! -z "$SUPER_TOKEN_NATIVE_COIN" ];then
    echo SUPER_TOKEN_NATIVE_COIN
    try_verify SETHProxy@${SUPER_TOKEN_NATIVE_COIN} --implementation SuperToken
fi

set +x

echo "Failed verifications:"
printf -- "- %s\n" "${FAILED_VERIFICATIONS[@]}"
exit ${#FAILED_VERIFICATIONS[@]}
