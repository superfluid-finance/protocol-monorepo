const { web3tx } = require("@decentral.ee/web3-test-helpers");
const configs = require("./configs");

module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const TestResolver = artifacts.require("TestResolver");
        const TestToken = artifacts.require("TestToken");
        const SuperToken = artifacts.require("SuperToken");
        const FlowAgreement = artifacts.require("FlowAgreement");

        const network = await web3.eth.net.getNetworkType();
        console.log("network: ", network);

        const config = configs[network];

        let testResolver;
        if (config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "Call: TestResolver.new")();
        }

        let testTokenAddress = await testResolver.get("TestToken");
        if (testTokenAddress === "0x0000000000000000000000000000000000000000") {
            const testToken = await web3tx(TestToken.new, "Call: TestToken.new")();
            testTokenAddress = testToken.address;
            console.log("TestToken address", testTokenAddress);
        }
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
