set -x

TRUFFLE_NETWORK=$1

echo SUPERFLUID_HOST
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_HOST_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan Superfluid@${SUPERFLUID_HOST_LOGIC}

echo SUPERFLUID_GOVERNANCE
npx truffle --network $TRUFFLE_NETWORK run etherscan TestGovernance@${SUPERFLUID_GOVERNANCE}

echo SUPERFLUID_SUPER_TOKEN_FACTORY
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}

echo SUPERFLUID_SUPER_TOKEN_LOGIC
# it is required to provide the constructor arguments manually, because the super token logic is created through a contract not an EOA
SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS=$(node -e 'console.log(("0".repeat(64)+("'${SUPERFLUID_HOST_PROXY}'".slice(2))).slice(-64))')
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperToken@${SUPERFLUID_SUPER_TOKEN_LOGIC} --forceConstructorArgs string:${SUPERFLUID_SUPER_TOKEN_LOGIC_CONSTRUCTOR_ARGS}

echo CFA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${CFA_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan ConstantFlowAgreementV1@${CFA_LOGIC}

echo IDA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${IDA_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan InstantDistributionAgreementV1@${IDA_LOGIC}

echo fDAIx
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FDAI}

echo fUSDCx
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FUSDC}

echo fTUSDx
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FTUSD}

echo ETHx
npx truffle --network $TRUFFLE_NETWORK run etherscan SETHProxy@${SUPER_TOKEN_ETHX}

set +x
