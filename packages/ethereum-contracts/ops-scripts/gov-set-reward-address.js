const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
    ZERO_ADDRESS,
} = require("./libs/common");

/**
 * @dev Set the reward address for a super token.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/gov-set-reward-address.js : {TOKEN ADDRESS} {REWARD ADDRESS}
 *        If REWARD ADDRESS is 0x0000000000000000000000000000000000000000,
 *        the reward address for the token is cleared and thus set to the default.
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Setting reward address ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const rewardAddr = args.pop();
    const tokenAddr = args.pop();
    console.log("token address", tokenAddr);
    console.log("reward address", rewardAddr);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "SuperfluidGovernanceBase",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    if (rewardAddr !== ZERO_ADDRESS) {
        console.log("setting new reward address");
        await sendGovernanceAction(sf, (gov) =>
            gov.setRewardAddress(sf.host.address, tokenAddr, rewardAddr)
        );
    } else {
        console.log("clearing reward address");
        await sendGovernanceAction(sf, (gov) =>
            gov.clearRewardAddress(sf.host.address, tokenAddr)
        );
    }
});
