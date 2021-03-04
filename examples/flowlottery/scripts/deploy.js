const { web3tx } = require("@decentral.ee/web3-helpers");
const { setWeb3Provider } = require("@decentral.ee/web3-helpers/src/config");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const LotterySuperApp = artifacts.require("LotterySuperApp");

module.exports = async function(callback) {
    try {
        const version = process.env.RELEASE_VERSION || "test";
        console.log("release version:", version);

        // make sure that we are using the same web3 provider in the helpers
        setWeb3Provider(web3.currentProvider);

        const sf = new SuperfluidSDK.Framework({
            web3,
            version: version,
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
