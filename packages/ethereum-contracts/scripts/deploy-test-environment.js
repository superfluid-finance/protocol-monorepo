const Web3 = require("web3");

const deployFramework = require("./deploy-framework");
const deployTestToken = require("./deploy-test-token");
const deploySuperToken = require("./deploy-super-token");

const { validateWeb3Arguments } = require("./utils");

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
    { isTruffle, web3, ethers, from } = {}
) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        validateWeb3Arguments({ web3, ethers, isTruffle });

        console.log("==== Deploying superfluid framework...");
        await deployFramework(errorHandler, {
            ethers,
            web3,
            from
        });
        console.log("==== Superfluid framework deployed.");

        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        await Promise.all([
            ...tokens.map(async token => {
                console.log(`==== Deploying test token ${token}...`);
                await deployTestToken(errorHandler, [":", token], {
                    ethers,
                    web3,
                    from
                });
                console.log(`==== Test token ${token} deployed.`);

                console.log(`==== Creating super token for ${token}...`);
                await deploySuperToken(errorHandler, [":", token], {
                    ethers,
                    web3,
                    from
                });
                console.log(`==== Super token for ${token} deployed.`);
            }),
            // Creating SETH
            deploySuperToken(errorHandler, [":", "ETH"], {
                ethers,
                web3,
                from
            })
        ]);

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
