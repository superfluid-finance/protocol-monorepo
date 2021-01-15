const Web3 = require("web3");

const deployFramework = require("./deploy-framework");
const deployTestToken = require("./deploy-test-token");
const deploySuperToken = require("./deploy-super-token");

/**
 * @dev Deploy the superfluid framework and test tokens for local testing
 * @param isTruffle (optional) Whether the script is used within the truffle framework
 * @param web3Provider (optional) The web3 provider to be used instead
 * @param from (optional) Address to deploy contracts from, use accounts[0] by default
 *
 * Usage: npx truffle exec scripts/deploy-test-environment.js
 */
module.exports = async function(
    callback,
    { isTruffle, web3Provider, from } = {}
) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        this.web3 = web3Provider ? new Web3(web3Provider) : web3;
        if (!this.web3) throw new Error("No web3 is available");

        console.log("==== Deploying superfluid framework...");
        await deployFramework(errorHandler, {
            isTruffle,
            web3Provider: this.web3.currentProvider,
            from
        });
        console.log("==== Superfluid framework deployed.");

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        for (let i = 0; i < tokens.length; ++i) {
            console.log(`==== Deploying test token ${tokens[i]}...`);
            await deployTestToken(errorHandler, [":", tokens[i]], {
                isTruffle,
                web3Provider: this.web3.currentProvider,
                from
            });
            console.log(`==== Test token ${tokens[i]} deployed.`);

            console.log(`==== Creating super token for ${tokens[i]}...`);
            await deploySuperToken(errorHandler, [":", tokens[i]], {
                isTruffle,
                web3Provider: this.web3.currentProvider,
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
