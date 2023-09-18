const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    ZERO_ADDRESS,
    extractWeb3Options,
    builtTruffleContractLoader,
    setResolver,
} = require("./libs/common");

/**
 * @dev Unlist a previously listed super token in resolver.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec ops-scripts/resolver-unlist-super-token.js : {SYMBOL}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Unlist a super token ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 1) {
        throw new Error("Wrong number of arguments");
    }
    const superTokenSymbol = args.pop();
    console.log("Super Token Symbol", superTokenSymbol);

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "ISafe",
            "SuperfluidGovernanceBase",
            "SuperToken",
            "Resolver",
            "IAccessControlEnumerable",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const superTokenKey = `supertokens.${protocolReleaseVersion}.${superTokenSymbol}`;
    console.log("Super token key", superTokenKey);

    const resolver = await sf.contracts.Resolver.at(sf.resolver.address);
    const superTokenAddress = await resolver.get.call(superTokenKey);

    if (superTokenAddress === ZERO_ADDRESS) {
        throw new Error("Super token is not listed");
    }

    await setResolver(sf, superTokenKey, ZERO_ADDRESS);
});
