const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const DividendRightsToken = artifacts.require("DividendRightsToken");

module.exports = async function(callback) {
    try {
        const version = process.env.RELEASE_VERSION || "test";
        console.log("release version:", version);

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
