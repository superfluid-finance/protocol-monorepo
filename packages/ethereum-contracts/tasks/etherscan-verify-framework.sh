set -x

echo SUPERFLUID_HOST
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_HOST_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan Superfluid@${SUPERFLUID_HOST_LOGIC}

echo SUPERFLUID_GOVERNANCE
npx truffle --network $TRUFFLE_NETWORK run etherscan TestGovernance@${SUPERFLUID_GOVERNANCE}

echo SUPERFLUID_SUPER_TOKEN_FACTORY
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPERFLUID_SUPER_TOKEN_FACTORY_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperTokenFactory@${SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC}

echo SUPERFLUID_SUPER_TOKEN_LOGIC
npx truffle --network $TRUFFLE_NETWORK run etherscan SuperToken@${SUPERFLUID_SUPER_TOKEN_LOGIC}

echo CFA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${CFA_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan ConstantFlowAgreementV1@${CFA_LOGIC}

echo IDA
npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${IDA_PROXY}
npx truffle --network $TRUFFLE_NETWORK run etherscan InstantDistributionAgreementV1@${IDA_LOGIC}

#echo FDAI
#npx truffle --network $TRUFFLE_NETWORK run etherscan TestToken@${TEST_TOKEN_FDAI}
#npx truffle --network $TRUFFLE_NETWORK run etherscan UUPSProxy@${SUPER_TOKEN_FDAI}

set +x
