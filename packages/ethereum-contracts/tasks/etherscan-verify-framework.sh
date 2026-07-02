#!/usr/bin/env bash

# verification script for etherscan-like explorers.
# takes 2 arguments: the canonical network name and a file with a list of contract addresses to verify.
# If additional arguments are provided, they will be added to individual verification commands.
# tries to verify the (sub)set of contracts listed in the file.
# if proxy addresses are provided, verification against up-to-date logic contracts will only succeed
# once they point to those (after gov upgrade execution)


TRUFFLE_NETWORK=$1
ADDRESSES_VARS=$2

shift 1

hardhat_artifact_path() {
    find build/hardhat -path "*/${1}.sol/${1}.json" ! -name "*.dbg.json" 2>/dev/null | head -1
}

if [ -z "$ADDRESSES_VARS" ]; then
    EXTRA_ARGS="$*"
    echo "EXTRA_ARGS: $EXTRA_ARGS"
    echo "no addresses provided, fetching myself..."
    ADDRESSES_VARS="/tmp/superfluid.$TRUFFLE_NETWORK.addrs"
    yarn run-hardhat run scripts/ops/run-info-print-contract-addresses.js --network "$TRUFFLE_NETWORK" -- : "$ADDRESSES_VARS" || exit 1
else
    shift 1
fi

EXTRA_ARGS="$*"

# shellcheck disable=SC1090
source "$ADDRESSES_VARS"

FAILED_VERIFICATIONS=()
function try_verify() {
    echo # newline for better readability
    cmd="npx hardhat verify --network $TRUFFLE_NETWORK $* ${EXTRA_ARGS:+$EXTRA_ARGS}"
    echo "> $cmd"
    $cmd || FAILED_VERIFICATIONS[${#FAILED_VERIFICATIONS[@]}]="$*"
}

function link_library() {
    local contract_name="$1"
    local library_name="$2"
    local library_address="$3"
    local artifact_path
    artifact_path=$(hardhat_artifact_path "$contract_name")

    if [ -z "$artifact_path" ]; then
        echo "artifact not found for $contract_name" >&2
        return 1
    fi

    echo "linking $contract_name to $library_name at $library_address"

    cp -f "$artifact_path" "${artifact_path}.bak"
    jq -s '.[0] * .[1]' \
        "${artifact_path}.bak" \
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
        ) > "$artifact_path"
}

if [ -n "$RESOLVER" ]; then
    try_verify Resolver@"${RESOLVER}"
fi

if [ -n "$ERC2771_FORWARDER" ]; then
    try_verify ERC2771Forwarder@"${ERC2771_FORWARDER}"
fi
if [ -n "$SIMPLE_FORWARDER" ]; then
    try_verify SimpleForwarder@"${SIMPLE_FORWARDER}"
fi

if [ -n "$SUPERFLUID_HOST_LOGIC" ]; then
    try_verify Superfluid@"${SUPERFLUID_HOST_LOGIC}"
fi
if [ -n "$SUPERFLUID_HOST_PROXY" ]; then
    try_verify Superfluid@"${SUPERFLUID_HOST_PROXY}" --contract contracts/superfluid/Superfluid.sol:Superfluid
fi

if [ -n "$SUPERFLUID_GOVERNANCE" ]; then
    if [ -n "$IS_TESTNET" ]; then
        try_verify TestGovernance@"${SUPERFLUID_GOVERNANCE}"
    else
        if [ -n "$SUPERFLUID_GOVERNANCE_LOGIC" ]; then
            try_verify SuperfluidGovernanceII@"${SUPERFLUID_GOVERNANCE_LOGIC}"
        fi
        try_verify SuperfluidGovernanceII@"${SUPERFLUID_GOVERNANCE}" --contract contracts/gov/SuperfluidGovernanceII.sol:SuperfluidGovernanceII
    fi
fi

if [ -n "$SUPERFLUID_LOADER" ]; then
    try_verify SuperfluidLoader@"${SUPERFLUID_LOADER}"
fi

if [ -n "$SUPER_TOKEN_FACTORY_LOGIC" ]; then
    try_verify SuperTokenFactory@"${SUPER_TOKEN_FACTORY_LOGIC}"
fi
if [ -n "$SUPER_TOKEN_FACTORY_PROXY" ]; then
    try_verify SuperTokenFactory@"${SUPER_TOKEN_FACTORY_PROXY}" --contract contracts/superfluid/SuperTokenFactory.sol:SuperTokenFactory
fi

if [ -n "$POOL_ADMIN_NFT_PROXY" ]; then
    try_verify PoolAdminNFT@"${POOL_ADMIN_NFT_PROXY}" --contract contracts/agreements/gdav1/PoolAdminNFT.sol:PoolAdminNFT
fi

if [ -n "$POOL_MEMBER_NFT_PROXY" ]; then
    try_verify PoolMemberNFT@"${POOL_MEMBER_NFT_PROXY}" --contract contracts/agreements/gdav1/PoolMemberNFT.sol:PoolMemberNFT
fi

if [ -n "$POOL_ADMIN_NFT_LOGIC" ]; then
    try_verify PoolAdminNFT@"${POOL_ADMIN_NFT_LOGIC}"
fi

if [ -n "$POOL_MEMBER_NFT_LOGIC" ]; then
    try_verify PoolMemberNFT@"${POOL_MEMBER_NFT_LOGIC}"
fi

if [ -n "$SUPER_TOKEN_LOGIC" ]; then
    try_verify SuperToken@"${SUPER_TOKEN_LOGIC}"
fi

if [ -n "$CFA_LOGIC" ]; then
    try_verify ConstantFlowAgreementV1@"${CFA_LOGIC}"
fi
if [ -n "$CFA_PROXY" ]; then
    try_verify ConstantFlowAgreementV1@"${CFA_PROXY}" --contract contracts/agreements/ConstantFlowAgreementV1.sol:ConstantFlowAgreementV1
fi

if [ -n "$SLOTS_BITMAP_LIBRARY" ]; then
    try_verify SlotsBitmapLibrary@"${SLOTS_BITMAP_LIBRARY}"
fi

link_library "InstantDistributionAgreementV1" "SlotsBitmapLibrary" "${SLOTS_BITMAP_LIBRARY}"
if [ -n "$IDA_LOGIC" ]; then
    try_verify InstantDistributionAgreementV1@"${IDA_LOGIC}"
fi
if [ -n "$IDA_PROXY" ]; then
    try_verify InstantDistributionAgreementV1@"${IDA_PROXY}" --contract contracts/agreements/InstantDistributionAgreementV1.sol:InstantDistributionAgreementV1
fi
ida_artifact=$(hardhat_artifact_path InstantDistributionAgreementV1)
if [ -n "$ida_artifact" ] && [ -f "${ida_artifact}.bak" ]; then
    mv -f "${ida_artifact}.bak" "$ida_artifact"
fi

if [ -n "$SUPERFLUID_POOL_DEPLOYER_LIBRARY" ]; then
    try_verify SuperfluidPoolDeployerLibrary@"${SUPERFLUID_POOL_DEPLOYER_LIBRARY}"
fi

if [ -n "$DUMMY_BEACON_PROXY" ]; then
    try_verify BeaconProxy@"${DUMMY_BEACON_PROXY}"
fi

link_library "GeneralDistributionAgreementV1" "SlotsBitmapLibrary" "${SLOTS_BITMAP_LIBRARY}"
link_library "GeneralDistributionAgreementV1" "SuperfluidPoolDeployerLibrary" "${SUPERFLUID_POOL_DEPLOYER_LIBRARY}"
if [ -n "$GDA_LOGIC" ]; then
    try_verify GeneralDistributionAgreementV1@"${GDA_LOGIC}"
fi

if [ -n "$GDA_PROXY" ]; then
    try_verify GeneralDistributionAgreementV1@"${GDA_PROXY}" --contract contracts/agreements/gdav1/GeneralDistributionAgreementV1.sol:GeneralDistributionAgreementV1
fi
gda_artifact=$(hardhat_artifact_path GeneralDistributionAgreementV1)
if [ -n "$gda_artifact" ] && [ -f "${gda_artifact}.bak" ]; then
    mv -f "${gda_artifact}.bak" "$gda_artifact"
fi

if [ -n "$SUPERFLUID_POOL_BEACON" ]; then
    try_verify SuperfluidUpgradeableBeacon@"${SUPERFLUID_POOL_BEACON}"
fi

if [ -n "$SUPERFLUID_POOL_LOGIC" ]; then
    try_verify SuperfluidPool@"${SUPERFLUID_POOL_LOGIC}"
fi

if [ -n "$SUPER_TOKEN_NATIVE_COIN" ];then
    try_verify SuperToken@"${SUPER_TOKEN_NATIVE_COIN}" --contract contracts/superfluid/SuperToken.sol:SuperToken
fi

for var in "${!NON_SUPER_TOKEN_@}"; do
    addr=${!var}
    try_verify TestToken@"$addr"
done

if [ -n "$CFAV1_FORWARDER" ];then
    try_verify CFAv1Forwarder@"${CFAV1_FORWARDER}"
fi

if [ -n "$GDAV1_FORWARDER" ];then
    try_verify GDAv1Forwarder@"${GDAV1_FORWARDER}"
fi

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
