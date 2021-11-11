const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./utils");

/**
 * @dev Set the reward address for a super token.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/gov-set-reward-address.js : {TOKEN ADDRESS} {REWARD ADDRESS}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Setting reward address ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 2) {
            throw new Error("Wrong number of arguments");
        }
        const rewardAddr = args.pop();
        const tokenAddr = args.pop();
        console.log("token address", tokenAddr);
        console.log("reward address", rewardAddr);

        protocolReleaseVersion =
            protocolReleaseVersion || process.env.RELEASE_VERSION || "test";
        console.log("protocol release version:", protocolReleaseVersion);

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

        await sendGovernanceAction(sf, (gov) =>
            gov.setRewardAddress(sf.host.address, tokenAddr, rewardAddr)
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
