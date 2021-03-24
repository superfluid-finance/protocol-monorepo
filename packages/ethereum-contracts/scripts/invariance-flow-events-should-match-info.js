const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    parseColonArgs,
    detectTruffleAndConfigure,
    extractWeb3Options,
} = require("./utils");

/**
 * @dev Invariance check: flow events should match flow info
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/invariance-flow-events-should-match-info.js : {SUPER_TOKEN_NAME}
 */
module.exports = async function (callback, argv, options = {}) {
    await eval(`(${detectTruffleAndConfigure.toString()})(options)`);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: process.env.RELEASE_VERSION || "test",
        loadSuperNativeToken: true,
    });
    await sf.initialize();

    const args = parseColonArgs(argv || process.argv);
    if (args.length !== 1) {
        throw new Error("Not enough arguments");
    }
    const superTokenName = args.pop();
    console.log("SuperToken name", superTokenName);
    await sf.loadToken(superTokenName);

    const flowUpdates = await sf.agreements.cfa.getPastEvents("FlowUpdated", {
        fromBlock: 0,
        toBlock: "latest",
        filter: {
            token: sf.supertokens[superTokenName],
        },
    });
    console.log(flowUpdates.lenght);
};
