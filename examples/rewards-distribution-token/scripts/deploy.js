const { web3tx } = require("@decentral.ee/web3-helpers");
const { setWeb3Provider } = require("@decentral.ee/web3-helpers/src/config");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const DividendRightsToken = artifacts.require("DividendRightsToken");

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

        const app = await web3tx(
            DividendRightsToken.new,
            "Deploy DividendRightsToken"
        )(
            "Dividend Rights Token",
            "DRT",
            sf.tokens.fDAIx.address,
            sf.host.address,
            sf.agreements.ida.address
        );
        console.log("App deployed at", app.address);
        callback();
    } catch (err) {
        callback(err);
    }
};
