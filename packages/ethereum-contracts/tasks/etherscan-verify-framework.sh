set -xe

TRUFFLE_NETWORK=$1

echo TRUFFLE_NETWORK=$TRUFFLE_NETWORK
echo NETWORK_ID=$NETWORK_ID

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
    bsc )
        echo "$TRUFFLE_NETWORK is mainnet"
        IS_TESTNET=
        ;;
    *)
        echo "Unknown network: $TRUFFLE_NETWORK"
        exit 1;
esac

#if [ "$TRUFFLE_NETWORK" == "arbitrum-rinkeby" ];then
#    NO_FORCE_CONSTRUCTOR_ARGS=1
#    echo "$TRUFFLE_NETWORK contract verification support is not stable, skpping it for now"
#    exit 0
#fi

echo SUPERFLUID_HOST
npx truffle --network $TRUFFLE_NETWORK run verify Superfluid@${SUPERFLUID_HOST_PROXY}


echo SUPERFLUID_GOVERNANCE
if [ ! -z "$IS_TESTNET" ];then
    npx truffle --network $TRUFFLE_NETWORK run verify TestGovernance@${SUPERFLUID_GOVERNANCE}
fi

echo SUPERFLUID_SUPER_TOKEN_FACTORY
npx truffle --network $TRUFFLE_NETWORK run verify SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}

echo SUPERFLUID_SUPER_TOKEN_LOGIC
if [ -z "$NO_FORCE_CONSTRUCTOR_ARGS" ];then
    # it is required to provide the constructor arguments manually, because the super token logic is created through a contract not an EOA
    SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS=$(node -e 'console.log("'${SUPERFLUID_HOST_PROXY}'".toLowerCase().slice(2).padStart(64, "0"))')
    npx truffle --network $TRUFFLE_NETWORK run verify SuperToken@${SUPERFLUID_SUPER_TOKEN_LOGIC} --forceConstructorArgs string:${SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS}
else
    echo "!!! WARNING !!! Cannot verify super token logic due to forceConstructorArgs not supported."
fi

echo CFA
npx truffle --network $TRUFFLE_NETWORK run verify ConstantFlowAgreementV1@${CFA_PROXY}

echo SlotsBitmapLibrary
npx truffle --network $TRUFFLE_NETWORK run verify SlotsBitmapLibrary@${SLOTS_BITMAP_LIBRARY_ADDRESS}

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
npx truffle --network $TRUFFLE_NETWORK run verify InstantDistributionAgreementV1@${IDA_LOGIC}
mv -f build/contracts/InstantDistributionAgreementV1.json.bak build/contracts/InstantDistributionAgreementV1.json

set +x
