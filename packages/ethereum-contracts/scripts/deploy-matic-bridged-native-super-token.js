const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    ZERO_ADDRESS,
} = require("./libs/common");

/**
 * @dev Deploy unlisted native super token to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-matic-bridged-native-super-token.js : {NAME} {SYMBOL} {CHILD_CHAIN_MANAGER}
 *        CHILD_CHAIN_MANAGER is the bridge contract account calling the deposit function which mints tokens
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("== Deploying unlisted Matic bridged native super token ==");
    let {protocolReleaseVersion} = options;

    if (args.length !== 3) {
        throw new Error("Wrong number of arguments");
    }
    const childChainManager = args.pop();
    const superTokenSymbol = args.pop();
    const superTokenName = args.pop();
    console.log("Super token name", superTokenName);
    console.log("Super token symbol", superTokenSymbol);

    if (!web3.utils.isAddress(childChainManager)) {
        throw new Error(`not a valid address: ${childChainManager}`);
    }
    console.log("Child chain manager", childChainManager);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "MaticBridgedNativeSuperTokenProxy",
            "IMaticBridgedNativeSuperToken",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const {MaticBridgedNativeSuperTokenProxy, IMaticBridgedNativeSuperToken} =
        sf.contracts;

    const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
        await sf.host.getSuperTokenFactory.call()
    );

    console.log("Deploying MaticBridgedNativeSuperTokenProxy...");
    const proxy = await MaticBridgedNativeSuperTokenProxy.new(
        childChainManager
    );

    const token = await IMaticBridgedNativeSuperToken.at(proxy.address);

    console.log("Invoking initializeCustomSuperToken...");
    await superTokenFactory.initializeCustomSuperToken(token.address);

    console.log("Invoking initialize...");
    await token.initialize(ZERO_ADDRESS, 18, superTokenName, superTokenSymbol);

    console.log(`Matic Bridged Native SuperToken deployed at ${token.address}`);
    return token.address;
});
