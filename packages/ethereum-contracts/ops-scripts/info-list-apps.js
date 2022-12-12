const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
} = require("./libs/common");

module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let {protocolReleaseVersion} = options;

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
    });
    await sf.initialize();

    const events = await sf.getPastEvents(sf.host, "AppRegistered");
    const apps = [];
    const froms = {};
    console.log("## Registered apps");
    console.log("DateTime | App | Address | From | Jailed");
    //for (let i = 0; i < events.length; ++i) {
    await Promise.all(
        events.map(async (e) => {
            //const e = events[i];
            const block = await web3.eth.getBlock(e.blockNumber);
            const receipt = await web3.eth.getTransactionReceipt(
                e.transactionHash
            );
            receipt !== null && froms[receipt.from]++;
            const isJailed = await sf.host.isAppJailed(e.app);
            apps.push({
                address: e.app,
                block,
                receipt,
                isJailed,
            });
        })
    );
    apps.sort((a, b) => a.block.timestamp - b.block.timestamp).forEach(
        (app) => {
            const date = new Date(app.block.timestamp * 1000);
            console.log(
                `${date.toISOString().slice(0, 19)} | ${app.address} | ${
                    (app.receipt || {}).from
                } | ${app.isJailed ? "*" : ""}`
            );
        }
    );
    console.log("---");
    console.log("Total apps registered", apps.length);
    console.log("Unique from addresses", Object.keys(froms).length);
});
