const SuperfluidSDK = require("..");
const { parseColonArgs } = require("./utils");


/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : {TOKEN_NAME}
 */
module.exports = async function (callback, argv) {
    try {
        global.web3 = web3;

        const chainId = await web3.eth.net.getId(); // TODO use eth.getChainId;
        const version = process.env.RELEASE_VERSION || "test";
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();

        global.artifacts = artifacts;
        const sf = new SuperfluidSDK.Framework({
            isTruffle: true,
            version
        });
        await sf.initialize();

        const tokenAddress = await sf.resolver.get(`tokens.${tokenName}`);
        const tokenInfo = await sf.contracts.TokenInfo.at(tokenAddress);
        const tokenInfoName = await tokenInfo.name.call();
        const tokenInfoSymbol = await tokenInfo.symbol.call();
        const tokenInfoDecimals = await tokenInfo.decimals.call();
        console.log("Token address", tokenAddress);
        console.log("Token name", tokenName);
        console.log("Token info name()", tokenInfoName);
        console.log("Token info symbol()", tokenInfoSymbol);
        console.log("Token info decimals()", tokenInfoDecimals.toString());

        const superTokenWrapper = await sf.getERC20Wrapper(tokenInfo);
        console.log("SuperToken wrapper address: ", superTokenWrapper.wrapperAddress);
        if (!superTokenWrapper.created) {
            console.log("Creating the wrapper...");
            await sf.createERC20Wrapper(tokenInfo);
            console.log("Wrapper created.");
        } else {
            console.log("SuperToken wrapper already created.");
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
