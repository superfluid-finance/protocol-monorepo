set -xe

TRUFFLE_NETWORK=$1

echo TRUFFLE_NETWORK=$TRUFFLE_NETWORK
echo NETWORK_ID=$NETWORK_ID

echo SUPERFLUID_HOST
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_HOST_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan Superfluid@${SUPERFLUID_HOST_LOGIC}

echo SUPERFLUID_GOVERNANCE
case $TRUFFLE_NETWORK in
    goerli | rinkeby | ropsten | kovan | mumbai )
        IS_TESTNET=1
esac

if [ ! -z "$IS_TESTNET" ];then
    npx truffle --network $TRUFFLE_NETWORK run etherscan TestGovernance@${SUPERFLUID_GOVERNANCE}
else
    npx truffle --network $TRUFFLE_NETWORK run etherscan SuperfluidOwnableGovernance@${SUPERFLUID_GOVERNANCE}
fi

echo SUPERFLUID_SUPER_TOKEN_FACTORY
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}

echo SUPERFLUID_SUPER_TOKEN_LOGIC
# it is required to provide the constructor arguments manually, because the super token logic is created through a contract not an EOA
SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS=$(node -e 'console.log("'${SUPERFLUID_HOST_PROXY}'".toLowerCase().slice(2).padStart(64, "0"))')
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperToken@${SUPERFLUID_SUPER_TOKEN_LOGIC} --forceConstructorArgs string:${SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS}

echo CFA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${CFA_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan ConstantFlowAgreementV1@${CFA_LOGIC}

echo SlotsBitmapLibrary
npx truffle --network $TRUFFLE_NETWORK run etherscan SlotsBitmapLibrary@${SLOTS_BITMAP_LIBRARY_ADDRESS}

echo IDA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${IDA_PROXY}
# HACK: do library link ourselves
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
npx truffle --network $TRUFFLE_NETWORK run etherscan InstantDistributionAgreementV1@${IDA_LOGIC}
mv -f build/contracts/InstantDistributionAgreementV1.json.bak build/contracts/InstantDistributionAgreementV1.json


if [ ! -z "${IS_TESTNET}" ];then
    echo fDAIx
    npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FDAIX}

    echo fUSDCx
    npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FUSDCX}

    echo fTUSDx
    npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FTUSDX}

    echo ETHx
    npx truffle --network $TRUFFLE_NETWORK run etherscan SETHProxy@${SUPER_TOKEN_ETHX}
fi

set +x
