const SuperfluidSDK = require("..");
const { parseColonArgs } = require("./utils");

function normalizeFlowRate(fr) {
    return (fr.toString() / 1e18 * 3600 * 24 * 30).toFixed(4) + " / mo";
}

function getLatestFlows(flows) {
    return Object.values(flows.reduce((acc, i) => {
        acc[i.args.sender + ":" + i.args.receiver] = i;
        return acc;
    }, {})).filter(i => i.args.flowRate.toString() != "0");
}

/**
 * @dev Inspect accounts and their agreements
 *
 * Usage: npx truffle exec scripts/inspect-account.js : 0xACC1 0xACC2 ...
 */
module.exports = async function (callback, argv) {
    global.web3 = web3;

    try {

        const args = parseColonArgs(argv || process.argv);
        if (args.length < 1) {
            throw new Error("Not enough arguments");
        }
        const sf = new SuperfluidSDK.Framework({
            chainId: 5,
            version:  process.env.RELEASE_VERSION || "test",
            web3Provider: web3.currentProvider
        });
        await sf.initialize();

        const tokens = ["fDAI"];
        while (args.length) {
            const account = args.shift();
            console.log("=".repeat(80));
            console.log("account", account);
            for (let i = 0; i < tokens.length; ++i) {
                const tokenName = tokens[i];
                const tokenAddress = await sf.resolver.get(`tokens.${tokenName}`);
                const token = await sf.contracts.ERC20WithTokenInfo.at(tokenAddress);
                const tokenWrapper = await sf.getERC20Wrapper(token);
                const superToken = await sf.contracts.ISuperToken.at(tokenWrapper.wrapperAddress);           
                console.log(`${tokenName} balance`, (await token.balanceOf.call(account)).toString() / 1e18);
                const realtimeBalance = await superToken.realtimeBalanceOf.call(account, parseInt(Date.now() / 1000));
                console.log(`${tokenName}x balance`,
                    realtimeBalance.availableBalance.toString() / 1e18,
                    realtimeBalance.deposit.toString() / 1e18,
                    realtimeBalance.owedDeposit.toString() / 1e18,
                );
                const netFlowRate = await sf.agreements.cfa.getNetFlow.call(superToken.address, account);
                console.log(`Net flowrate ${normalizeFlowRate(netFlowRate)}`);
                console.log("In Flows:");
                console.log(getLatestFlows(await sf.agreements.cfa.getPastEvents("FlowUpdated", {
                    fromBlock: 0,
                    filter: {
                        receiver: account
                    }
                })).map(f => `${f.args.sender} -> ${normalizeFlowRate(f.args.flowRate)}`));
                console.log("Out Flows:");
                console.log(getLatestFlows(await sf.agreements.cfa.getPastEvents("FlowUpdated", {
                    fromBlock: 0,
                    filter: {
                        sender: account
                    }
                })).map(f => `${f.args.sender} -> ${normalizeFlowRate(f.args.flowRate)}`));
            }
            console.log("=".repeat(80));
        }
        
        callback();

    } catch (err) {
        callback(err);
    }      
};

