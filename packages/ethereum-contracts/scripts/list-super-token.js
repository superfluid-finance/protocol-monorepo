const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    ZERO_ADDRESS,
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
    setResolver,
} = require("./utils");

/**
 * @dev List a super token in resolver.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/list-super-token : {SUPER_TOKEN_ADDRESS}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== List new super token ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const superTokenAddress = args.pop();
        console.log("Super Token Address", superTokenAddress);

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
                "SuperToken",
                "TestResolver",
            ],
            contractLoader: builtTruffleContractLoader,
        });
        await sf.initialize();

        const superToken = await sf.contracts.SuperToken.at(superTokenAddress);
        if (
            (await superToken.proxiableUUID.call()) !==
            web3.utils.sha3(
                "org.superfluid-finance.contracts.SuperToken.implementation"
            )
        ) {
            throw new Error("Not a super token");
        }
        const tokenSymbol = await superToken.symbol.call();
        const superTokenKey = `supertokens.${protocolReleaseVersion}.${tokenSymbol}`;
        console.log("Super token key", superTokenKey);

        const resolver = await sf.contracts.TestResolver.at(
            sf.resolver.address
        );
        if ((await resolver.get.call(superTokenKey)) !== ZERO_ADDRESS) {
            throw new Error("Super token already listed");
        }
        await setResolver(sf, superTokenKey, superTokenAddress);

        callback();
    } catch (err) {
        callback(err);
    }
};
