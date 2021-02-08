const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const LotterySuperApp = artifacts.require("LotterySuperApp");

module.exports = async function(callback, argv) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        global.web3 = web3;

        const version = process.env.RELEASE_VERSION || "test";
        console.log("release version:", version);

        const sf = new SuperfluidSDK.Framework({
            version,
            web3Provider: web3.currentProvider,
            tokens: ["fDAI"]
        });
        await sf.initialize();

        const app = await web3tx(LotterySuperApp.new, "Deploy LotterySuperApp")(
            sf.host.address,
            sf.agreements.cfa.address,
            sf.tokens.fDAIx.address
        );
        console.log("App deployed at", app.address);
        callback();
    } catch (err) {
        callback(err);
    }
};
