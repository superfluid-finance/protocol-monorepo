/**
 * @dev Deploy the superfluid framework and test tokens for local testing
 *
 * Usage: npx truffle exec scripts/deploy-test-environment.js
 */

module.exports = async function(callback, { isTruffle, from } = {}) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        global.web3 = web3;

        const deployFramework = require("./deploy-framework");
        const deployTestToken = require("./deploy-test-token");
        const deploySuperToken = require("./deploy-super-token");

        console.log("==== Deploying superfluid framework...");
        await deployFramework(errorHandler, { isTruffle });
        console.log("==== Superfluid framework deployed.");

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        for (let i = 0; i < tokens.length; ++i) {
            console.log(`==== Deploying test token ${tokens[i]}...`);
            await deployTestToken(errorHandler, [":", tokens[i]], {
                isTruffle,
                from
            });
            console.log(`==== Test token ${tokens[i]} deployed.`);

            console.log(`==== Creating super token for ${tokens[i]}...`);
            await deploySuperToken(errorHandler, [":", tokens[i]], {
                isTruffle,
                from
            });
            console.log(`==== Super token for ${tokens[i]} deployed.`);
        }

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
