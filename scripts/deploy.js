const { web3tx } = require("@decentral.ee/web3-helpers");
const Superfluid = require("..");

const TestResolver = artifacts.require("TestResolver");
const SuperfluidRegistry = artifacts.require("SuperfluidRegistry");
const TestToken = artifacts.require("TestToken");
const TestGovernance = artifacts.require("TestGovernance");
const Proxy = artifacts.require("Proxy");
const SuperToken = artifacts.require("SuperToken");
const FlowAgreement = artifacts.require("FlowAgreement");

async function hasCode(address) {
    const code = await web3.eth.getCode(address);
    return code !== "0x" && code !== "";
}

async function codeChanged(contract, address) {
    const code = await web3.eth.getCode(address);
    // SEE: https://github.com/ConsenSys/bytecode-verifier/blob/master/src/verifier.js
    // find the second occurance of the init code
    const startingPoint = contract.bytecode.slice(11).indexOf("6080604052") + 11;
    const bytecodeFromCompiler = contract.bytecode.slice(startingPoint);
    return code.slice(2) !== bytecodeFromCompiler;
}

async function proxiableCodeChanged(contract, address) {
    const p = await Proxy.at(address);
    return await codeChanged(contract, await p.getCodeAddress());
}

/**
 * @dev Deploy the superfluid framework, and one super token
 */
module.exports = async function (callback) {
    const ZERO_ADDRESS = "0x0000000000000000000000000000000000000000";

    try {
        global.web3 = web3;

        const accounts = await web3.eth.getAccounts();

        const reset = !!process.env.RESET;
        const version = process.env.RELEASE_VERSION || "test";

        const chainId = await web3.eth.net.getId(); // TODO use eth.getChainId;
        console.log("reset: ", reset);
        console.log("chain ID: ", chainId);
        console.log("release version:", version);

        const config = Superfluid.getConfig(chainId);

        let testResolver;
        if (config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
        }
        console.log("Resolver address", testResolver.address);

        // deploy SuperfluidRegistry
        let registry;
        {
            const name = `SuperfluidRegistry.${version}`;
            let registryAddress = await testResolver.get(name);
            console.log("SuperfluidRegistry address", registryAddress);
            if (reset || !await hasCode(registryAddress)) {
                const proxy = await web3tx(Proxy.new, "Create SuperfluidRegistry proxy")();
                registryAddress = proxy.address;
                const registryLogic = await web3tx(SuperfluidRegistry.new, "SuperfluidRegistry.new")();
                console.log(`SuperfluidRegistry new code address ${registryLogic.address}`);
                await web3tx(proxy.initializeProxy, "proxy.initializeProxy")(
                    registryLogic.address
                );
                const registry = await SuperfluidRegistry.at(proxy.address);
                await web3tx(registry.initialize, "SuperfluidRegistry.initialize")(
                    ZERO_ADDRESS
                );
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, proxy.address
                );
                console.log("SuperfluidRegistry address", registryAddress);
            } else {
                if (await proxiableCodeChanged(SuperfluidRegistry, registryAddress)) {
                    const registryLogic = await web3tx(SuperfluidRegistry.new,
                        "SuperfluidRegistry.new due to code change")();
                    console.log(`SuperfluidRegistry new code address ${registryLogic.address}`);
                    const registry = await SuperfluidRegistry.at(registryAddress);
                    await web3tx(registry.updateCode, "registry.updateCode")(
                        registryLogic.address
                    );
                } else {
                    console.log("SuperfluidRegistry has the same logic code, no deployment needed.");
                }
            }
            registry = await SuperfluidRegistry.at(registryAddress);
        }

        // deploy FlowAgreement
        {
            const name = `FlowAgreement.${version}`;
            const flowAgreementAddress = await testResolver.get(name);
            console.log("FlowAgreement address", flowAgreementAddress);
            if (reset || await codeChanged(FlowAgreement, flowAgreementAddress)) {
                const agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new due to code change")();
                console.log("New FlowAgreement address", agreement.address);
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, agreement.address
                );
            } else {
                console.log("FlowAgreement has the same code, no deployment needed");
            }
        }

        // deploy TestGovernance
        let governanceAddress;
        {
            const name = `TestGovernance.${version}`;
            governanceAddress = await testResolver.get(name);
            console.log("TestGovernance address", governanceAddress);
            if (reset || await codeChanged(TestGovernance, governanceAddress)) {
                const governance = await web3tx(TestGovernance.new, "TestGovernance.new due to code change")(
                    accounts[0],
                    2,
                    3600
                );
                governanceAddress = governance.address;
                console.log("TestGovernance address", governance.address);
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, governance.address
                );
            } else {
                console.log("TestGovernance has the same code, no deployment needed.");
            }
        }

        // update governance address
        if (await registry.getGovernance() !== governanceAddress){
            await web3tx(registry.setGovernance, "registry.setGovernance")(
                governanceAddress
            );
        }

        // deploy test token and its super token
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
