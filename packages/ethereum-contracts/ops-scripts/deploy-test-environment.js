const fs = require("fs");
const util = require("util");
const getConfig = require("./libs/getConfig");
const deployFramework = require("./deploy-framework");
const deployTestToken = require("./deploy-test-token");
const deploySuperToken = require("./deploy-super-token");

const {getScriptRunnerFactory: S} = require("./libs/common");

async function takeEvmSnapshot() {
    return new Promise((resolve, reject) => {
        web3.currentProvider.send(
            {
                jsonrpc: "2.0",
                method: "evm_snapshot",
                params: [],
            },
            (err, result) => {
                if (err) {
                    return reject(err);
                }
                return resolve(result.result);
            }
        );
    });
}

/**
 * @dev Deploy the superfluid framework and test tokens for local testing
 *
 * Usage: npx truffle exec scripts/deploy-test-environment.js : {TOKENS_TO_DEPLOY}
 * where TOKENS_TO_DEPLOY is an optional list of token symbols, e.g. TEST1,TEST2.
 * For each such token, an underlying ERC20 and a wrapping Super Token will be deployed.
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    const errorHandler = (err) => {
        if (err) throw err;
    };

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);

    let tokens;

    if (args.length >= 1) {
        tokens = args.pop().split(",");
    } else {
        tokens = ["fDAI", "fUSDC", "fTUSD", config.nativeTokenSymbol];
    }
    console.log("Tokens to be deployed", tokens);

    console.log("======== Deploying superfluid framework ========");
    await deployFramework(errorHandler, options);
    console.log("==== Superfluid framework deployed  ========");

    for (let i = 0; i < tokens.length; ++i) {
        if (tokens[i] !== deploySuperToken) {
            console.log(`======== Deploying test token ${tokens[i]} ========`);
            await deployTestToken(errorHandler, [":", tokens[i]], options);
            console.log(`======== Test token ${tokens[i]} deployed ========`);
        }

        console.log(`======== Creating super token for ${tokens[i]} ========`);
        await deploySuperToken(errorHandler, [":", tokens[i]], options);
        console.log(`======== Super token for ${tokens[i]} deployed ========`);
    }

    if (process.env.RESOLVER_ADDRESS) {
        console.log(
            "=============== TEST ENVIRONMENT VARS ======================"
        );
        console.log(`export RESOLVER_ADDRESS=${process.env.RESOLVER_ADDRESS}`);
    }

    if (process.env.TESTENV_SNAPSHOT_VARS) {
        let output = "";
        output += `RESOLVER_ADDRESS=${process.env.RESOLVER_ADDRESS}\n`;
        output += `TESTENV_EVM_SNAPSHOT_ID=${await takeEvmSnapshot()}\n`;
        await util.promisify(fs.writeFile)(
            process.env.TESTENV_SNAPSHOT_VARS,
            output
        );
    }
});
