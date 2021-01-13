const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { parseColonArgs } = require("./utils");

function normalizeFlowRate(fr) {
    return ((fr.toString() / 1e18) * 3600 * 24 * 30).toFixed(4) + " / mo";
}

/**
 * @dev Inspect accounts and their agreements
 *
 * Usage: npx truffle exec scripts/inspect-account.js : 0xACC1 0xACC2 ...
 */
module.exports = async function(callback, argv) {
    try {
        const args = parseColonArgs(argv || process.argv);
        if (args.length < 1) {
            throw new Error("Not enough arguments");
        }
        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        const sf = new SuperfluidSDK.Framework({
            version: process.env.RELEASE_VERSION || "test",
            web3Provider: web3.currentProvider,
            tokens
        });
        await sf.initialize();

        while (args.length) {
            const account = args.shift();
            console.log("=".repeat(80));
            console.log("account", account);
            for (let i = 0; i < tokens.length; ++i) {
                console.log("-".repeat(80));
                const tokenName = tokens[i];
                const token = sf.tokens[tokenName];
                const superToken = sf.tokens[tokenName + "x"];
                console.log(
                    `${tokenName} balance`,
                    (await token.balanceOf.call(account)).toString() / 1e18
                );
                const realtimeBalance = await superToken.realtimeBalanceOf.call(
                    account,
                    parseInt(Date.now() / 1000)
                );
                console.log(
                    `${tokenName}x balance`,
                    realtimeBalance.availableBalance.toString() / 1e18,
                    realtimeBalance.deposit.toString() / 1e18,
                    realtimeBalance.owedDeposit.toString() / 1e18
                );
                const netFlowRate = await sf.cfa.getNetFlow({
                    superToken: superToken.address,
                    account
                });
                console.log(`Net flow rate ${normalizeFlowRate(netFlowRate)}`);
                const flows = await sf.cfa.listFlows({
                    superToken: superToken.address,
                    account
                });
                console.log("In Flows:");
                console.log(
                    flows.inFlows.map(
                        f => `${f.sender} -> ${normalizeFlowRate(f.flowRate)}`
                    )
                );
                console.log("Out Flows:");
                console.log(
                    flows.outFlows.map(
                        f => `${f.sender} -> ${normalizeFlowRate(f.flowRate)}`
                    )
                );
            }
            console.log("=".repeat(80));
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
