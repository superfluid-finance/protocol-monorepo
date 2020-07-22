const { web3tx } = require("@decentral.ee/web3-helpers");
const Superfluid = require("..");

async function codeChanged(contract, address) {
    const code = await web3.eth.getCode(address);
    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    const startingPoint = contract.bytecode.lastIndexOf("6080604052");
    const bytecodeFromCompiler = contract.bytecode.slice(startingPoint);
    return code.slice(2) !== bytecodeFromCompiler;
}

module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const accounts = await web3.eth.getAccounts();

        const TestResolver = artifacts.require("TestResolver");
        const TestToken = artifacts.require("TestToken");
        const TestGovernance = artifacts.require("TestGovernance");
        const Proxy = artifacts.require("Proxy");
        const SuperToken = artifacts.require("SuperToken");
        const FlowAgreement = artifacts.require("FlowAgreement");

        const reset = !!process.env.RESET;
        const version = process.env.RELEASE_VERSION || "test";
        const netId = await web3.eth.net.getId();
        console.log("reset: ", reset);
        console.log("network ID: ", netId);
        console.log("release version:", version);

        const config = Superfluid.getConfig(netId);

        let testResolver;
        if (config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        }
        console.log("Resolver address", testResolver.address);

        {
            const name = `FlowAgreement.${version}`;
            const flowAgreementAddress = await testResolver.get(name);
            if (reset || await codeChanged(FlowAgreement, flowAgreementAddress)) {
                const agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")();
                console.log("FlowAgreement address", agreement.address);
                await web3tx(testResolver.set, `TestResolver set FlowAgreement.${version}`)(
                    name, agreement.address
                );
            } else {
                console.log(`Current FlowAgreement address ${flowAgreementAddress}. Same code, no deployment neede`);
            }
        }

        let governanceAddress;
        {
            const name = `TestGovernance.${version}`;
            governanceAddress = await testResolver.get(name);
            if (reset || await codeChanged(TestGovernance, governanceAddress)) {
                const governance = await web3tx(TestGovernance.new, "TestGovernance.new")(
                    accounts[0],
                    2,
                    3600
                );
                governanceAddress = governance.address;
                console.log("TestGovernance address", governance.address);
                await web3tx(testResolver.set, `TestResolver set TestGovernance.${version}`)(
                    name, governance.address
                );
            } else {
                console.log(`Current TestGovernance address ${governanceAddress}. Same code, no deployment neede`);
            }
        }

        let testTokenAddress = await testResolver.get(`TestToken.${version}`);
        if (reset || testTokenAddress === "0x0000000000000000000000000000000000000000") {
            const testToken = await web3tx(TestToken.new, "TestToken.new")();
            testTokenAddress = testToken.address;
            await web3tx(testResolver.set, `TestResolver set TestToken.${version}`)(
                `TestToken.${version}`, testTokenAddress
            );
        }
        console.log("TestToken address", testTokenAddress);

        const superTokenLogic = await web3tx(SuperToken.new, "Create super token logic contract")();

        let superTestTokenAddress = await testResolver.get(`SuperTestToken.${version}`);
        if (reset || superTestTokenAddress === "0x0000000000000000000000000000000000000000") {
            const proxy = await web3tx(Proxy.new, "Create super token proxy contract")();
            await web3tx(proxy.initializeProxy, "proxy.initializeProxy")(
                superTokenLogic.address
            );
            const superToken = await SuperToken.at(proxy.address);
            await web3tx(superToken.initialize, "superToken.initialize")(
                "SuperTestToken",
                "STT",
                18,
                testTokenAddress,
                governanceAddress
            );
            superTestTokenAddress = proxy.address;
            await web3tx(testResolver.set, `TestResolver set superTestToken.${version}`)(
                `SuperTestToken.${version}`, proxy.address
            );
            console.log("SuperTestToken address", superTestTokenAddress);
        } else {
            console.log("SuperTestToken address", superTestTokenAddress);
            const superTestToken = await SuperToken.at(superTestTokenAddress);
            await web3tx(superTestToken.updateCode, "Update super token logic contract")(
                superTokenLogic.address
            );
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
