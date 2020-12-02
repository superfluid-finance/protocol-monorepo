const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("..");

const TestResolver = artifacts.require("TestResolver");
const Superfluid = artifacts.require("Superfluid");
const SuperfluidMock = artifacts.require("SuperfluidMock");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");
const SuperTokenFactoryMock = artifacts.require("SuperTokenFactoryMock");
const TestGovernance = artifacts.require("TestGovernance");
const Proxy = artifacts.require("UUPSProxy");
const Proxiable = artifacts.require("UUPSProxiable");
const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
const InstantDistributionAgreementV1 = artifacts.require("InstantDistributionAgreementV1");

const {
    hasCode,
    codeChanged,
    isProxiable,
    proxiableCodeChanged
} = require("./utils");


/**
 * @dev Deploy the superfluid framework
 *
 * Usage: npx truffle exec scripts/deploy-framework.js
 */
module.exports = async function (callback, {
    newTestResolver,
    useMocks,
    nonUpgradable
} = {}) {
    try {
        global.web3 = web3;

        const accounts = await web3.eth.getAccounts();

        const reset = !!process.env.RESET;
        const version = process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // FIXME use eth.getChainId;
        console.log("reset: ", reset);
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        useMocks = useMocks || process.env.USE_MOCKS;
        nonUpgradable = nonUpgradable || process.env.NON_UPGRADABLE;
        if (useMocks) console.log("**** !ATTN! USING MOCKS CONTRACTS ****");
        if (nonUpgradable) console.log("**** !ATTN! DISABLED UPGRADABILITY ****");

        const config = SuperfluidSDK.getConfig(chainId);

        let testResolver;
        if (!newTestResolver && config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
            // make it available for the sdk for testing purpose
            process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        }
        console.log("Resolver address", testResolver.address);

        // deploy TestGovernance
        let governance;
        {
            const name = `TestGovernance.${version}`;
            const governanceAddress = await testResolver.get(name);
            console.log("TestGovernance address", governanceAddress);
            if (reset || await codeChanged(TestGovernance, governanceAddress)) {
                governance = await web3tx(TestGovernance.new, "TestGovernance.new due to code change")(
                    accounts[0], // rewardAddress
                    3600, // liquidationPeriod
                );
                console.log("TestGovernance address", governance.address);
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, governance.address
                );
            } else {
                governance = await TestGovernance.at(governanceAddress);
                console.log("TestGovernance has the same code, no deployment needed.");
            }
        }

        // deploy Superfluid
        let superfluid;
        {
            const name = `Superfluid.${version}`;
            let superfluidAddress = await testResolver.get(name);
            console.log("Superfluid address", superfluidAddress);
            const SuperfluidLogic = useMocks ? SuperfluidMock : Superfluid;
            if (reset || !await hasCode(superfluidAddress)) {
                const superfluidLogic = await web3tx(SuperfluidLogic.new, "SuperfluidLogic.new")(
                    nonUpgradable
                );
                console.log(`Superfluid new code address ${superfluidLogic.address}`);
                if (!nonUpgradable) {
                    const proxy = await web3tx(Proxy.new, "Create Superfluid proxy")();
                    await web3tx(proxy.initializeProxy, "proxy.initializeProxy")(
                        superfluidLogic.address
                    );
                    superfluidAddress = proxy.address;
                } else {
                    superfluidAddress = superfluidLogic.address;
                }
                await web3tx(testResolver.set, `TestResolver set ${name}`)(
                    name, superfluidAddress
                );
                superfluid = await Superfluid.at(superfluidAddress);
                console.log("Superfluid address", superfluidAddress);
                await web3tx(superfluid.initialize, "Superfluid.initialize")(
                    governance.address
                );
            } else {
                superfluid = await Superfluid.at(superfluidAddress);
            }
            superfluid = await Superfluid.at(superfluidAddress);

            if ((await superfluid.getGovernance.call()) !== governance.address){
                await web3tx(governance.replaceGovernance, "governance.replaceGovernance")(
                    superfluid.address, governance.address
                );
            }

            if (!nonUpgradable) {
                if (await proxiableCodeChanged(Proxiable, SuperfluidLogic, superfluidAddress)) {
                    console.log("Superfluid code has changed");
                    if (!(await isProxiable(Proxiable, superfluidAddress))) {
                        throw new Error("Superfluid is non-upgradable");
                    }
                    const superfluidLogic = await web3tx(SuperfluidLogic.new, "Superfluid.new due to code change")(
                        false /* nonUpgradable = false, of course... */
                    );
                    console.log(`Superfluid new code address ${superfluidLogic.address}`);
                    await web3tx(governance.updateHostCode, "governance.updateHostCode")(
                        superfluidAddress,
                        superfluidLogic.address
                    );
                } else {
                    console.log("Superfluid has the same logic code, no deployment needed.");
                }
            }
        }

        let superTokenFactoryLogicAddress;
        {
            superTokenFactoryLogicAddress = await superfluid.getSuperTokenFactoryLogic.call();
            console.log("superTokenFactory logic address", superTokenFactoryLogicAddress);
            const SuperTokenFactoryLogic = useMocks ? SuperTokenFactoryMock : SuperTokenFactory;
            if (reset || await codeChanged(SuperTokenFactoryLogic, superTokenFactoryLogicAddress)) {
                const superTokenLogic = await web3tx(
                    SuperTokenFactoryLogic.new,
                    "SuperTokenFactoryLogic.new due to code change")();
                superTokenFactoryLogicAddress = superTokenLogic.address;
                console.log("New superTokenFactory logic address", superTokenFactoryLogicAddress);
                await web3tx(governance.updateSuperTokenFactory, "superfluid.updateSuperTokenFactory")(
                    superfluid.address,
                    superTokenFactoryLogicAddress
                );
            } else {
                console.log("superTokenFactory has the same code, no deployment needed.");
            }
        }

        // deploy ConstantFlowAgreementV1
        {
            const agreementType = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
            const isListed = await superfluid.isAgreementTypeListed.call(agreementType);
            const isChanged = !isListed || await codeChanged(
                ConstantFlowAgreementV1,
                await (await Proxiable.at(await superfluid.getAgreementClass.call(agreementType))).getCodeAddress()
            );
            console.log(`ConstantFlowAgreementV1 isListed ${isListed} isChanged ${isChanged}`);
            if (reset || !isListed || isChanged) {
                const agreement = await web3tx(
                    ConstantFlowAgreementV1.new,
                    "ConstantFlowAgreementV1.new")();
                console.log("New ConstantFlowAgreementV1 address", agreement.address);
                if (reset || !isListed) {
                    await web3tx(governance.registerAgreementClass, "Governance registers CFA")(
                        superfluid.address,
                        agreement.address
                    );
                } else {
                    await web3tx(governance.updateAgreementClass, "Governance updates CFA code")(
                        superfluid.address,
                        agreement.address
                    );
                }
            } else {
                console.log("ConstantFlowAgreementV1 has the same code, no deployment needed");
            }
        }

        // deploy InstantDistributionAgreementV1
        {
            const agreementType = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");
            const isListed = await superfluid.isAgreementTypeListed.call(agreementType);
            const isChanged = !isListed || await codeChanged(
                InstantDistributionAgreementV1,
                await (await Proxiable.at(await superfluid.getAgreementClass.call(agreementType))).getCodeAddress()
            );
            console.log(`InstantDistributionAgreementV1 isListed ${isListed} isChanged ${isChanged}`);
            if (reset || !isListed || isChanged) {
                const agreement = await web3tx(
                    InstantDistributionAgreementV1.new,
                    "InstantDistributionAgreementV1.new")();
                console.log("New InstantDistributionAgreementV1 address", agreement.address);
                if (reset || !isListed) {
                    await web3tx(governance.registerAgreementClass, "Governance registers IDA")(
                        superfluid.address,
                        agreement.address
                    );
                } else {
                    await web3tx(governance.updateAgreementClass, "Governance updates IDA code")(
                        superfluid.address,
                        agreement.address
                    );
                }
            } else {
                console.log("InstantDistributionAgreementV1 has the same code, no deployment needed");
            }
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
