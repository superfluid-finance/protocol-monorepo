const { web3tx } = require("@decentral.ee/web3-helpers");
const { setWeb3Provider } = require("@decentral.ee/web3-helpers/src/config");
const TestResolver = artifacts.require("TestResolver");
const getConfig = require("./getConfig");

const { parseColonArgs, rl } = require("./utils");

/**
 * @dev Reset the superfluid framework deployment.
 * @param {Array} argv Overriding command line arguments
 *
 * Usage: npx truffle exec scripts/resolver-reset-deployment.js : {VERSION}
 */
module.exports = async function (callback, argv) {
    try {
        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const version = args.pop();

        if (version != (await rl("Please confirm the version: "))) {
            console.error("Mismatched versions");
            callback();
        }

        const networkType = await this.web3.eth.net.getNetworkType();
        const networkId = await web3.eth.net.getId();
        const chainId = await this.web3.eth.getChainId();
        console.log("network Type: ", networkType);
        console.log("network ID: ", networkId);
        console.log("chain ID: ", chainId);
        const config = getConfig(chainId);

        // make sure that we are using the same web3 provider in the helpers
        setWeb3Provider(web3.currentProvider);

        let testResolver;
        if (config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
            // make it available for the sdk for testing purpose
            process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        }
        console.log("Resolver address", testResolver.address);

        await web3tx(testResolver.set, "Clear Superfluid deployment")(
            `Superfluid.${version}`,
            "0x" + "0".repeat(40)
        );
        await web3tx(testResolver.set, "Clear TestGovernance deployment")(
            `TestGovernance.${version}`,
            "0x" + "0".repeat(40)
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
