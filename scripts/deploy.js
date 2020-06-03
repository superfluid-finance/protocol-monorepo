const { web3tx } = require("@decentral.ee/web3-test-helpers");
const configs = require("./configs");

module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const TestResolver = artifacts.require("TestResolver");
        const SuperToken = artifacts.require("SuperToken");
        const FlowAgreement = artifacts.require("FlowAgreement");

        const network = await web3.eth.net.getNetworkType();
        console.log("network: ", network);

        const config = configs[network];
        const testResolver = await TestResolver.at(config.resolver.address);

        const testTokenAddress = await testResolver.get("TestToken");
        console.log("TestToken address", testTokenAddress);

        const agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")();

        console.log("FlowAgreement address", agreement.address);

        const superTestToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            testTokenAddress,
            "SuperTestToken",
            "STT");
        console.log("SuperTestToken address", superTestToken.address);

        await web3tx(testResolver.set, "TestResolver set FlowAgreement")(
            "FlowAgreement", agreement.address
        );
        await web3tx(testResolver.set, "TestResolver set superTestToken")(
            "SuperTestToken", superTestToken.address
        );

        //await superToken.addAgreement(agreement.address);

        callback();
    } catch (err) {
        callback(err);
    }
};
