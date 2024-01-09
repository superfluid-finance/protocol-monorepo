#!/usr/bin/env bash

set -xe

TRUFFLE_NETWORK=$1

## FIXME --verify-proxy is not an option yet

echo TRUFFLE_NETWORK="$TRUFFLE_NETWORK"
echo NETWORK_ID="$NETWORK_ID"
echo TRUFFLE_RUN_VERIFY="npx truffle run --network $TRUFFLE_NETWORK verify"

echo SUPERFLUID_HOST UUPSProxy
$TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${SUPERFLUID_HOST_PROXY}"

echo SUPER_TOKEN_FACTORY UUPSProxy
$TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${SUPER_TOKEN_FACTORY_PROXY}"

echo CFA UUPSProxy
$TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${CFA_PROXY}"

echo IDA UUPSProxy
$TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${IDA_PROXY}"

if [ -n "${IS_TESTNET}" ];then
    echo fDAIx
    $TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${SUPER_TOKEN_FDAIX}"

    echo fUSDCx
    $TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${SUPER_TOKEN_FUSDCX}"

    echo fTUSDx
    $TRUFFLE_RUN_VERIFY --verify-proxy UUPSProxy@"${SUPER_TOKEN_FTUSDX}"

    echo ETHx
    $TRUFFLE_RUN_VERIFY --verify-proxy SETHProxy@"${SUPER_TOKEN_NATIVE_COIN}"
fi

set +x
