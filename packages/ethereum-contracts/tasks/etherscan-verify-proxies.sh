set -xe

TRUFFLE_NETWORK=$1

echo TRUFFLE_NETWORK=$TRUFFLE_NETWORK
echo NETWORK_ID=$NETWORK_ID

echo SUPERFLUID_HOST UUPSProxy
npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${SUPERFLUID_HOST_PROXY}

echo SUPERFLUID_SUPER_TOKEN_FACTORY UUPSProxy
npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}

echo CFA UUPSProxy
npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${CFA_PROXY}

echo IDA UUPSProxy
npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${IDA_PROXY}

if [ ! -z "${IS_TESTNET}" ];then
    echo fDAIx
    npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${SUPER_TOKEN_FDAIX}

    echo fUSDCx
    npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${SUPER_TOKEN_FUSDCX}

    echo fTUSDx
    npx truffle --network $TRUFFLE_NETWORK run verify UUPSProxy@${SUPER_TOKEN_FTUSDX}

    echo ETHx
    npx truffle --network $TRUFFLE_NETWORK run verify SETHProxy@${SUPER_TOKEN_NATIVE_COIN}
fi

set +x
