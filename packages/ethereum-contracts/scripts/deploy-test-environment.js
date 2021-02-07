const deployFramework = require("./deploy-framework");
const deployTestToken = require("./deploy-test-token");
const deploySuperToken = require("./deploy-super-token");

const { detectTruffleAndConfigure } = require("./utils");

/**
 * @dev Deploy the superfluid framework and test tokens for local testing
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-test-environment.js
 */
module.exports = async function(callback, options = {}) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        console.log("==== Deploying superfluid framework...");
        await deployFramework(errorHandler, options);
        console.log("==== Superfluid framework deployed.");

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        for (let i = 0; i < tokens.length; ++i) {
            console.log(`==== Deploying test token ${tokens[i]}...`);
            await deployTestToken(errorHandler, [":", tokens[i]], options);
            console.log(`==== Test token ${tokens[i]} deployed.`);

            console.log(`==== Creating super token for ${tokens[i]}...`);
            await deploySuperToken(errorHandler, [":", tokens[i]], options);
            console.log(`==== Super token for ${tokens[i]} deployed.`);
        }
        // Creating SETH
        await deploySuperToken(errorHandler, [":", "ETH"], options);

        if (process.env.TEST_RESOLVER_ADDRESS) {
            console.log(
                "=============== TEST ENVIRONMENT RESOLVER ======================"
            );
            console.log(
                `export TEST_RESOLVER_ADDRESS=${process.env.TEST_RESOLVER_ADDRESS}`
            );
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
