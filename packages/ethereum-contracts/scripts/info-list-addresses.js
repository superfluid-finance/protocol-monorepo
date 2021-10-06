const async = require("async");
const fs = require("fs");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { setupScriptEnvironment, extractWeb3Options } = require("./utils");

const MAX_REQUESTS = 200;

module.exports = async function (callback, argv, options = {}) {
    try {
        await eval(`(${setupScriptEnvironment.toString()})(options)`);

        let { protocolReleaseVersion } = options;

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: protocolReleaseVersion,
            tokens: ["MATICx"],
        });
        await sf.initialize();

        const latestBlock = await web3.eth.getBlock("latest");

        if (!fs.existsSync("addresses.ignore.data")) {
            const accounts = new Set();
            const startingBlockNumber = 11650607;
            const rangeStep = 1e4;
            for (
                let fromBlock = startingBlockNumber;
                fromBlock <= latestBlock.number;
                fromBlock += rangeStep
            ) {
                const toBlock = Math.min(
                    fromBlock + rangeStep,
                    latestBlock.number
                );
                console.log("Query", fromBlock, toBlock);
                const events = [
                    ...(await sf.tokens.MATICx.getPastEvents(
                        "AgreementStateUpdated",
                        {
                            fromBlock: "0x" + fromBlock.toString(16),
                            toBlock: "0x" + toBlock.toString(16),
                        }
                    )),
                    ...(await sf.tokens.MATICx.getPastEvents("TokenUpgraded", {
                        fromBlock: "0x" + fromBlock.toString(16),
                        toBlock: "0x" + toBlock.toString(16),
                    })),
                ];
                events.forEach((e) =>
                    accounts.add(e.args.account.toLowerCase())
                );
            }

            fs.writeFileSync(
                "addresses.ignore.data",
                Array.from(accounts).join("\n")
            );
        }

        const accounts = fs
            .readFileSync("addresses.ignore.data")
            .toString()
            .split("\n");
        console.log("Number of accounts", accounts.length);
        const balances = await async.mapLimit(
            accounts,
            MAX_REQUESTS,
            async (account) => {
                const rtb = await sf.tokens.MATICx.realtimeBalanceOf.call(
                    account,
                    latestBlock.timestamp,
                    latestBlock.number
                );
                return {
                    account,
                    availableBalance: rtb.availableBalance.toString(),
                };
            }
        );
        const balancesSum = balances.reduce((acc, cur) => {
            return acc.add(web3.utils.toBN(cur.availableBalance));
        }, web3.utils.toBN(0));
        console.log("Balances sum", balancesSum.toString() / 1e18);

        callback();
    } catch (err) {
        callback(err);
    }
};
