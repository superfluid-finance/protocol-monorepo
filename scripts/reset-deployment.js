const { web3tx } = require("@decentral.ee/web3-helpers");
const TestResolver = artifacts.require("TestResolver");
const SuperfluidSDK = require("..");

const {
    parseColonArgs,
    rl
} = require("./utils");

/**
 * @dev Reset the superfluid framework deployment.
 *
 * Usage: npx truffle exec scripts/reset-deployment.js : {VERSION}
 */
module.exports = async function (callback, argv) {
    try {
        global.web3 = web3;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const version = args.pop();

        if (version != await rl("Please confirm the version: ")) {
            console.error("Mismatched versions");
            callback();
        }

        const chainId = await web3.eth.net.getId(); // FIXME use eth.getChainId;

        const config = SuperfluidSDK.getConfig(chainId);

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
            `Superfluid.${version}`, "0x" + "0".repeat(40)
        );
        await web3tx(testResolver.set, "Clear TestGovernance deployment")(
            `TestGovernance.${version}`, "0x" + "0".repeat(40)
        );

        callback();
    } catch (err) {
        callback(err);
    }
};

