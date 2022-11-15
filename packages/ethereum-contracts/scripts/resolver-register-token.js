const {web3tx} = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const getConfig = require("./libs/getConfig");

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
} = require("./libs/common");

/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.resetToken Reset the token deployment
 *
 * Usage: npx truffle exec scripts/resolver-register-token.js : {TOKEN_NAME} {TOKEN_ADDRESS}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Register test token ========");
    let {resetToken} = options;

    if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const tokenAddress = args.pop();
    const tokenName = args.pop();
    console.log("Token name", tokenName);
    console.log("Token address", tokenAddress);

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);

    resetToken = resetToken || !!process.env.RESET_TOKEN;
    console.log("reset token: ", resetToken);
    console.log("chain ID: ", chainId);

    const {Resolver} = await SuperfluidSDK.loadContracts({
        ...extractWeb3Options(options),
        additionalContracts: ["Resolver"],
        contractLoader: builtTruffleContractLoader,
        networkId,
    });

    const resolver = await Resolver.at(config.resolverAddress);
    console.log("Resolver address", resolver.address);

    const name = `tokens.${tokenName}`;
    let testTokenAddress = await resolver.get(name);

    if (
        resetToken ||
        testTokenAddress === "0x0000000000000000000000000000000000000000"
    ) {
        await web3tx(resolver.set, `Resolver set ${name}`)(name, tokenAddress);
    } else {
        console.log("Token already set");
    }

    console.log("======== Test token registered ======");
});
