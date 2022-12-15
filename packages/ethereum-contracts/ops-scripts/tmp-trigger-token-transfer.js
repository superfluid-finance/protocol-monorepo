const {getScriptRunnerFactory: S} = require("./libs/common");

const SuperToken = artifacts.require("SuperToken");

/**
 * @dev Trigger a token Transfer event in order to have the subgraph index it.
 * Also needed for a change of the resolver listing state to take effect.
 * TODO: remove once not needed anymore
 *
 * Usage: npx truffle exec ops-scripts/tmp-trigger-token-transfer.js : {TOKEN_ADDRESS}
 */
module.exports = eval(`(${S.toString()})()`)(async function (args) {
    console.log("======== Trigger token Transfer ========");

    if (args.length !== 1) {
        throw new Error("Wrong number of arguments");
    }
    const tokenAddr = args.pop();
    console.log("Super Token Address", tokenAddr);

    const token = await SuperToken.at(tokenAddr);
    const tokenSymbol = await token.symbol();

    console.log("Token Symbol:", tokenSymbol);

    // transfer 0 tokens to some address
    const r = await token.transfer(
        "0x1111111111111111111111111111111111111111",
        0
    );

    console.log("done in", r.tx);
});
