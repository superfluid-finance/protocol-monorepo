const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
} = require("./utils");

/**
 * @dev Deploy unlisted native super token to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-native-super-token.js : {NAME} ${SYMBOL} ${INITIAL SUPPLY}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Deploying unlisted native super token ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 3) {
            throw new Error("Wrong number of arguments");
        }
        const initialSupply = args.pop();
        const superTokenSymbol = args.pop();
        const superTokenName = args.pop();
        console.log("Super token name", superTokenName);
        console.log("Super token symbol", superTokenSymbol);
        console.log("Super token initial supply", initialSupply);

        protocolReleaseVersion =
            protocolReleaseVersion || process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("chain ID: ", chainId);
        console.log("protocol release version:", protocolReleaseVersion);

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: protocolReleaseVersion,
            additionalContracts: ["NativeSuperTokenProxy", "INativeSuperToken"],
            contractLoader: builtTruffleContractLoader,
        });
        await sf.initialize();

        const { NativeSuperTokenProxy, INativeSuperToken } = sf.contracts;

        const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
            await sf.host.getSuperTokenFactory.call()
        );

        console.log("Deploying NativeSuperTokenProxy...");
        const proxy = await NativeSuperTokenProxy.new();

        const token = await INativeSuperToken.at(proxy.address);

        console.log("Invoking initializeCustomSuperToken...");
        await superTokenFactory.initializeCustomSuperToken(token.address);

        console.log("Invoking initialize...");
        await token.initialize(
            superTokenName,
            superTokenSymbol,
            web3.utils.toWei(String(initialSupply))
        );

        console.log(`Native SuperToken deployed at ${token.address}`);

        callback();
    } catch (err) {
        callback(err);
    }
};
