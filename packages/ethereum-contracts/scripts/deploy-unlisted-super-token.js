const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
} = require("./utils");

/**
 * @dev Deploy unlisted super token to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-unmanaged-super-token.js : ${UNDERLYING_TOKEN_ADDRESS} {NAME} ${SYMBOL}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Deploying unmanaged super token ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { resetToken, protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 3) {
            throw new Error("Not enough arguments");
        }
        const superTokenSymbol = args.pop();
        const superTokenName = args.pop();
        const tokenAddress = args.pop();
        console.log("Underlying token address", tokenAddress);
        console.log("Super token name", superTokenName);
        console.log("Super token symbol", superTokenSymbol);

        resetToken = resetToken || !!process.env.RESET_TOKEN;
        protocolReleaseVersion =
            protocolReleaseVersion || process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("reset token: ", resetToken);
        console.log("chain ID: ", chainId);
        console.log("protocol release version:", protocolReleaseVersion);

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: protocolReleaseVersion,
            additionalContracts: ["UUPSProxiable"],
            contractLoader: builtTruffleContractLoader,
        });
        await sf.initialize();

        const tokenInfo = await sf.contracts.TokenInfo.at(tokenAddress);
        const tokenInfoName = await tokenInfo.name.call();
        const tokenInfoSymbol = await tokenInfo.symbol.call();
        const tokenInfoDecimals = await tokenInfo.decimals.call();
        console.log("Underlying token name", tokenInfoName);
        console.log("Underlying token info name()", tokenInfoName);
        console.log("Underlying token info symbol()", tokenInfoSymbol);
        console.log(
            "Underlying token info decimals()",
            tokenInfoDecimals.toString()
        );
        console.log("Creating the wrapper...");
        const superToken = await sf.createERC20Wrapper(tokenInfo, {
            superTokenName,
            superTokenSymbol,
        });
        console.log("Wrapper created at", superToken.address);

        console.log("======== Super token deployed ========");
        callback();
    } catch (err) {
        callback(err);
    }
};
