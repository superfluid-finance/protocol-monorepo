const { web3tx } = require("@decentral.ee/web3-test-helpers");
const configs = require("./configs");

module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const SuperToken = artifacts.require("SuperToken");
        const FlowAgreement = artifacts.require("FlowAgreement");

        const network = await web3.eth.net.getNetworkType();
        console.log("network: ", network);

        const config = configs[network];

        console.log("Token address", config.token.address);

        const agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")({
            gas: 1500000,
        });

        console.log("FlowAgreement address", agreement.address);

        const superToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            config.token.address,
            "SuperToken",
            "STK", {
                gas: 2600000
            });
        console.log("SuperToken address", superToken.address);

        await superToken.addAgreement(agreement.address);

        callback();
    } catch (err) {
        callback(err);
    }
};
