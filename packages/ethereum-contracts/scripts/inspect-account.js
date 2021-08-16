const getConfig = require("./getConfig");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { parseColonArgs } = require("./utils");

function normalizeFlowRate(fr) {
    return ((fr.toString() / 1e18) * 3600 * 24 * 30).toFixed(4) + " / mo";
}

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 *
 * Usage: npx truffle exec scripts/inspect-account.js : 0xACC1 0xACC2 ...
 */
module.exports = async function (callback, argv) {
    try {
        const args = parseColonArgs(argv || process.argv);
        if (args.length < 1) {
            throw new Error("Not enough arguments");
        }

        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("chain ID: ", chainId);
        const config = getConfig(chainId);

        const tokens = config.tokenList;
        const sf = new SuperfluidSDK.Framework({
            version: process.env.RELEASE_VERSION || "test",
            web3,
            tokens,
            loadSuperNativeToken: true,
        });
        await sf.initialize();
        const superTokens = Object.keys(sf.superTokens);

        while (args.length) {
            const account = args.shift();
            console.log("=".repeat(80));
            const isApp = await sf.host.isApp(account);
            console.log("Account", account, isApp ? "(app)" : "");
            if (isApp) {
                console.log("Jailed", await sf.host.isAppJailed(account));
            }
            for (let i = 0; i < superTokens.length; ++i) {
                console.log("-".repeat(80));
                const superTokenSymbol = superTokens[i];
                const superToken = sf.superTokens[superTokenSymbol];
                const underlyingToken = superToken.underlyingToken;
                if (underlyingToken) {
                    const underlyingTokenSymbol =
                        await underlyingToken.symbol();
                    console.log(
                        `${underlyingTokenSymbol} balance`,
                        (
                            await underlyingToken.balanceOf.call(account)
                        ).toString() / 1e18
                    );
                }
                const realtimeBalance = await superToken.realtimeBalanceOf.call(
                    account,
                    parseInt(Date.now() / 1000)
                );
                console.log(
                    `${superTokenSymbol} balance`,
                    realtimeBalance.availableBalance.toString() / 1e18,
                    realtimeBalance.deposit.toString() / 1e18,
                    realtimeBalance.owedDeposit.toString() / 1e18
                );
                {
                    console.log("# CFA");
                    const netFlowRate = await sf.cfa.getNetFlow({
                        superToken: superToken.address,
                        account,
                    });
                    console.log(
                        `Net flow rate ${normalizeFlowRate(netFlowRate)}`
                    );
                    const flows = await sf.cfa.listFlows({
                        superToken: superToken.address,
                        account,
                    });
                    console.log("In Flows:");
                    console.log(
                        flows.inFlows.map(
                            (f) =>
                                `${f.sender} -> ${normalizeFlowRate(
                                    f.flowRate
                                )}`
                        )
                    );
                    console.log("Out Flows:");
                    console.log(
                        flows.outFlows.map(
                            (f) =>
                                `${f.receiver} -> ${normalizeFlowRate(
                                    f.flowRate
                                )}`
                        )
                    );
                }
                {
                    console.log("# IDA");
                    console.log("Indicies:");
                    const indices = await sf.ida.listIndices({
                        superToken: superToken.address,
                        publisher: account,
                    });
                    console.log(indices);
                    console.log("Subscriptions:");
                    const subscriptions = await sf.ida.listSubscriptions({
                        superToken: superToken.address,
                        subscriber: account,
                    });
                    console.log(
                        subscriptions.map(
                            (s) =>
                                `${s.publisher}@${s.indexId} ${s.units} units`
                        )
                    );
                }
            }
            console.log("=".repeat(80));
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
