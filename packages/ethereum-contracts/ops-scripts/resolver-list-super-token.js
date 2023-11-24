const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    ZERO_ADDRESS,
    extractWeb3Options,
    builtTruffleContractLoader,
    setResolver,
} = require("./libs/common");

/**
 * @dev List a super token in resolver.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec ops-scripts/resolver-list-super-token.js : {SUPER_TOKEN_ADDRESS} [SYMBOL]
 *        If the optional argument SYMBOL is provided, it's used instead of the on-chain symbol.
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== List new super token ========");
    let {resetToken, protocolReleaseVersion} = options;

    if (args.length < 1 || args.length > 2) {
        throw new Error("Wrong number of arguments");
    }

    let symbolOverride = undefined;
    if (args.length === 2) {
        symbolOverride = args.pop();
        console.log("Symbol override", symbolOverride);
    }

    const superTokenAddress = args.pop();
    console.log("Super Token Address", superTokenAddress);

    resetToken = resetToken || !!process.env.RESET_TOKEN;
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

    const superToken = await sf.contracts.SuperToken.at(superTokenAddress);
    if (
        (await superToken.proxiableUUID.call()) !==
        web3.utils.sha3(
            "org.superfluid-finance.contracts.SuperToken.implementation"
        )
    ) {
        // This may be ok, but may also point to a mistake in address handling (e.g. not a SuperToken).
        console.warn('!!! proxiableUUID is not keccak("org.superfluid-finance.contracts.SuperToken.implementation")');
    }
    const tokenSymbol = symbolOverride !== undefined ? symbolOverride : await superToken.symbol.call();
    const superTokenKey = `supertokens.${protocolReleaseVersion}.${tokenSymbol}`;
    console.log("Super token key", superTokenKey);

    const resolver = await sf.contracts.Resolver.at(sf.resolver.address);
    if (
        (await resolver.get.call(superTokenKey)) !== ZERO_ADDRESS &&
        !resetToken
    ) {
        console.error("### Super token already listed, you can force overwrite with reset flag!");
        console.error("A Transfer event may be needed for indexers to notice.");
    } else {
        await setResolver(sf, superTokenKey, superTokenAddress);
    }
});
