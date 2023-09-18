const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
} = require("./libs/common");

/**
 * @dev Deploy unlisted super token to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-unmanaged-super-token.js : {UNDERLYING_TOKEN_ADDRESS} {NAME} {SYMBOL}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Deploying unmanaged super token ========");
    let {resetToken, protocolReleaseVersion} = options;

    if (args.length !== 3) {
        throw new Error("Wrong number of arguments");
    }
    const superTokenSymbol = args.pop();
    const superTokenName = args.pop();
    const tokenAddress = args.pop();
    console.log("Underlying token address", tokenAddress);
    console.log("Super token name", superTokenName);
    console.log("Super token symbol", superTokenSymbol);

    resetToken = resetToken || !!process.env.RESET_TOKEN;
    console.log("reset token: ", resetToken);
    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: ["UUPSProxiable", "IERC20Metadata"],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const iERC20Metadata = await sf.contracts.IERC20Metadata.at(tokenAddress);
    const name = await iERC20Metadata.name.call();
    const symbol = await iERC20Metadata.symbol.call();
    const decimals = await iERC20Metadata.decimals.call();
    console.log("Underlying token name", name);
    console.log("Underlying token info name()", name);
    console.log("Underlying token info symbol()", symbol);
    console.log("Underlying token info decimals()", decimals.toString());
    console.log("Creating the wrapper...");
    const superToken = await sf.createERC20Wrapper(iERC20Metadata, {
        superTokenName,
        superTokenSymbol,
    });
    console.log("Wrapper created at", superToken.address);

    console.log("======== Super token deployed ========");
    return superToken.address;
});
