const { web3tx } = require("@decentral.ee/web3-helpers");
const { setWeb3Provider } = require("@decentral.ee/web3-helpers/src/config");
const Resolver = artifacts.require("Resolver");
const getConfig = require("./getConfig");

const { parseColonArgs, rl } = require("./utils");

/**
 * @dev Reset the superfluid framework deployment.
 * @param {Array} argv Overriding command line arguments
 *
 * Usage: npx truffle exec scripts/reset-deployment.js : {VERSION}
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

        const networkId = await web3.eth.net.getId();
        console.log("network ID: ", networkId);
        // make sure that we are using the same web3 provider in the helpers
        setWeb3Provider(web3.currentProvider);

        const config = getConfig(networkId);

        let resolver;
        if (config.resolverAddress) {
            resolver = await Resolver.at(config.resolverAddress);
        } else {
            resolver = await web3tx(Resolver.new, "Resolver.new")();
            // make it available for the sdk for testing purpose
            process.env.RESOLVER_ADDRESS = resolver.address;
        }
        console.log("Resolver address", resolver.address);

        await web3tx(resolver.set, "Clear Superfluid deployment")(
            `Superfluid.${version}`,
            "0x" + "0".repeat(40)
        );
        await web3tx(resolver.set, "Clear TestGovernance deployment")(
            `TestGovernance.${version}`,
            "0x" + "0".repeat(40)
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
