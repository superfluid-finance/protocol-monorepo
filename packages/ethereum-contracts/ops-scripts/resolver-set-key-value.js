const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    ZERO_ADDRESS,
    extractWeb3Options,
    builtTruffleContractLoader,
    setResolver,
} = require("./libs/common");

/**
 * @dev Set a given value for a given key in resolver
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec ops-scripts/resolver-set-key-value.js : {KEY} {VALUE}
 *
 * ENV vars:
 *    ALLOW_UPDATE: only if set will existing values be overwritten
 *    RESOLVER_ADMIN_TYPE: needs to be set to MULTISIG for networks with multisig owner
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Set key with value ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const value = args.pop();
    const key = args.pop();

    console.log("Key", key);
    console.log("Value", value);

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "ISafe",
            "SuperfluidGovernanceBase",
            "Resolver",
            "IAccessControlEnumerable",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const resolver = await sf.contracts.Resolver.at(sf.resolver.address);

    const prevVal = await resolver.get.call(key);
    if (prevVal !== ZERO_ADDRESS) {
        console.log("Key already has a value", prevVal);
        if (!process.env.ALLOW_UPDATE) {
            throw new Error(
                "ALLOW_UPDATE not set, can't override existing value"
            );
        }
    }

    await setResolver(sf, key, value);
});
