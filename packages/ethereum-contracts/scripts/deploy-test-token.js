const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader
} = require("./utils");

/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-test-token.js : {TOKEN_NAME}
 */
module.exports = async function(callback, argv, options = {}) {
    try {
        console.log("Deploying test token");

        eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();
        console.log("Token name", tokenName);

        const reset = !!process.env.RESET_TOKEN;
        const chainId = await this.web3.eth.net.getId(); // TODO use eth.getChainId;
        const config = SuperfluidSDK.getConfig(chainId);
        console.log("reset: ", reset);
        console.log("chain ID: ", chainId);

        const { TestResolver, TestToken } = await SuperfluidSDK.loadContracts({
            ...extractWeb3Options(options),
            additionalContracts: ["TestResolver", "TestToken"],
            contractLoader: builtTruffleContractLoader
        });

        const testResolver = await TestResolver.at(config.resolverAddress);
        console.log("Resolver address", testResolver.address);

        // deploy test token and its super token
        const name = `tokens.${tokenName}`;
        let testTokenAddress = await testResolver.get(name);
        if (
            reset ||
            testTokenAddress === "0x0000000000000000000000000000000000000000"
        ) {
            const testToken = await web3tx(TestToken.new, "TestToken.new")(
                tokenName + " Fake Token",
                tokenName,
                18
            );
            testTokenAddress = testToken.address;
            await web3tx(testResolver.set, `TestResolver set ${name}`)(
                name,
                testTokenAddress
            );
        } else {
            console.log("Token already deployed");
        }
        console.log(`Token ${tokenName} address`, testTokenAddress);

        callback();
    } catch (err) {
        callback(err);
    }
};
