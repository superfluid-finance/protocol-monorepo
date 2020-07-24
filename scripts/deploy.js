const { web3tx } = require("@decentral.ee/web3-helpers");
const Superfluid = require("..");

const TestResolver = artifacts.require("TestResolver");
const SuperToken = artifacts.require("SuperToken");
const SuperfluidRegistry = artifacts.require("SuperfluidRegistry");
const TestGovernance = artifacts.require("TestGovernance");
const Proxy = artifacts.require("Proxy");
const Proxiable = artifacts.require("Proxiable");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    hasCode,
    codeChanged,
    proxiableCodeChanged
} = require("./utils");


/**
 * @dev Deploy the superfluid framework
 *
 * Usage: npx truffle exec scripts/deploy.js
 */
module.exports = async function (callback) {
    try {
        global.web3 = web3;

        const accounts = await web3.eth.getAccounts();

        const reset = !!process.env.RESET;
        const version = process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // FIXME use eth.getChainId;
        console.log("reset: ", reset);
        console.log("network ID: ", chainId);
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
                await web3tx(registry.initialize, "SuperfluidRegistry.initialize")();
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, proxy.address
                );
                console.log("SuperfluidRegistry address", registryAddress);
            } else {
                if (await proxiableCodeChanged(Proxiable, SuperfluidRegistry, registryAddress)) {
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

        let superTokenLogicAddress;
        {
            const name = `SuperTokenLogic.${version}`;
            superTokenLogicAddress = await testResolver.get(name);
            console.log("SuperTokenLogic address", superTokenLogicAddress);
            if (reset || await codeChanged(SuperToken, superTokenLogicAddress)) {
                const superTokenLogic = await web3tx(SuperToken.new, "SuperToken.new due to code change")();
                superTokenLogicAddress = superTokenLogic.address;
                console.log("SuperTokenLogic address", superTokenLogicAddress);
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, superTokenLogicAddress
                );
            } else {
                console.log("SuperTokenLogic has the same code, no deployment needed.");
            }
        }

        // update registry settings
        if ((await registry.getGovernance.call()) !== governanceAddress){
            await web3tx(registry.setGovernance, "registry.setGovernance")(
                governanceAddress
            );
        }
        if ((await registry.getSuperTokenLogic.call()) !== superTokenLogicAddress){
            await web3tx(registry.setSuperTokenLogic, "registry.setSuperTokenLogic")(
                superTokenLogicAddress
            );
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
