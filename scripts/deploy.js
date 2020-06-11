const { web3tx } = require("@decentral.ee/web3-test-helpers");
const configs = require("./configs");

module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const accounts = await web3.eth.getAccounts();

        const TestResolver = artifacts.require("TestResolver");
        const TestToken = artifacts.require("TestToken");
        const TestGovernance = artifacts.require("TestGovernance");
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
        console.log("Resolver address", testResolver.address);

        let testTokenAddress = await testResolver.get("TestToken");
        if (testTokenAddress === "0x0000000000000000000000000000000000000000") {
            const testToken = await web3tx(TestToken.new, "Call: TestToken.new")();
            testTokenAddress = testToken.address;
            await web3tx(testResolver.set, "TestResolver set TestToken")(
                "TestToken", testTokenAddress
            );
        }
        console.log("TestToken address", testTokenAddress);

        const agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")();

        console.log("FlowAgreement address", agreement.address);

        const superTestToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            testTokenAddress,
            "SuperTestToken",
            "STT");
        console.log("SuperTestToken address", superTestToken.address);

        const governance = await web3tx(TestGovernance.new, "Call: TestGovernance.new")(
            superTestToken.address,
            accounts[0],
            2,
            3600
        );

        await web3tx(testResolver.set, "TestResolver set FlowAgreement")(
            "FlowAgreement", agreement.address
        );
        await web3tx(testResolver.set, "TestResolver set superTestToken")(
            "SuperTestToken", superTestToken.address
        );
        await web3tx(testResolver.set, "TestResolver set TestGovernance")(
            "TestGovernance", governance.address
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
