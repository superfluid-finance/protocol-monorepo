const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("..");
const { parseColonArgs } = require("./utils");

const TestResolver = artifacts.require("TestResolver");
const TokenInfo = artifacts.require("TokenInfo");
const Superfluid = artifacts.require("Superfluid");


/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : {TOKEN_NAME}
 */
module.exports = async function (callback, argv) {
    try {
        global.web3 = web3;

        const chainId = await web3.eth.net.getId(); // TODO use eth.getChainId;
        const config = SuperfluidSDK.getConfig(chainId);
        const version = process.env.RELEASE_VERSION || "test";
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();

        const testResolver = await TestResolver.at(config.resolverAddress);
        console.log("Resolver address", testResolver.address);

        const tokenAddress = await testResolver.get(`tokens.${tokenName}`);
        const tokenInfo = await TokenInfo.at(tokenAddress);
        const tokenInfoName = await tokenInfo.name.call();
        const tokenInfoSymbol = await tokenInfo.symbol.call();
        const tokenInfoDecimals = await tokenInfo.decimals.call();
        console.log("Token address", tokenAddress);
        console.log("Token name", tokenName);
        console.log("Token info name()", tokenInfoName);
        console.log("Token info symbol()", tokenInfoSymbol);
        console.log("Token info decimals()", tokenInfoDecimals.toString());

        const superfluidAddress = await testResolver.get(`Superfluid.${version}`);
        const superfluid = await Superfluid.at(superfluidAddress);
        const superTokenWrapper = await SuperfluidSDK.getERC20Wrapper(superfluid, tokenInfo);
        console.log("SuperToken wrapper address: ", superTokenWrapper.wrapperAddress);
        if (!superTokenWrapper.created) {
            await web3tx(superfluid.createERC20Wrapper, "registry.createERC20Wrapper")(
                `Super ${tokenInfoName}`,
                `${tokenInfoSymbol}x`,
                tokenInfoDecimals,
                tokenAddress
            );
        } else {
            console.log("SuperToken wrapper already created.");
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
