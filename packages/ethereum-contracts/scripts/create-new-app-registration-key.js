const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./utils");

/**
 * @dev Create a new super app registration key.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/create-new-app-registration-key : {DEPLOYER} {REGISTRATION_KEY}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Creating new app registration key ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 2) {
            throw new Error("Not enough arguments");
        }
        const registrationkey = args.pop();
        const deployer = args.pop();
        console.log("Deployer", deployer);

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

        const secretKey = web3.utils.sha3(
            web3.eth.abi.encodeParameters(
                ["string", "address", "string"],
                [
                    "org.superfluid-finance.superfluid.appWhiteListing.seed",
                    deployer,
                    registrationkey,
                ]
            )
        );
        console.log("Secret key", secretKey);

        await sendGovernanceAction(sf, (gov) =>
            gov.whiteListNewApp(sf.host.address, secretKey)
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
