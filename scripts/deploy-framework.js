const { web3tx } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("..");

const TestResolver = artifacts.require("TestResolver");
const Superfluid = artifacts.require("Superfluid");
const SuperfluidMock = artifacts.require("SuperfluidMock");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");
const SuperTokenFactoryMock = artifacts.require("SuperTokenFactoryMock");
const TestGovernance = artifacts.require("TestGovernance");
const ISuperfluidGovernance = artifacts.require("ISuperfluidGovernance");
const Proxy = artifacts.require("UUPSProxy");
const Proxiable = artifacts.require("UUPSProxiable");
const ConstantFlowAgreementV1 = artifacts.require("ConstantFlowAgreementV1");
const InstantDistributionAgreementV1 = artifacts.require("InstantDistributionAgreementV1");

const {
    ZERO_ADDRESS,
    hasCode,
    codeChanged,
    isProxiable
} = require("./utils");


let reset;
let testResolver;

async function deployAndRegisterContractIf(Contract, resolverKey, cond, deployFunc) {
    let contractDeployed;
    const contractName = Contract.contractName;
    const contractAddress = await testResolver.get(resolverKey);
    console.log(`${resolverKey} address`, contractAddress);
    if (reset || await cond(contractAddress)) {
        console.log(`${contractName} needs new deployment.`);
        contractDeployed = await deployFunc();
        console.log(`${resolverKey} deployed to`, contractDeployed.address);
        await web3tx(testResolver.set, `Resolver set ${resolverKey}`)(
            resolverKey, contractDeployed.address
        );
    } else {
        console.log(`${contractName} does not need new deployment.`);
        contractDeployed = await Contract.at(contractAddress);
    }
    return contractDeployed;
}

async function deployNewLogicContractIfNew(LogicContract, codeAddress, deployFunc) {
    let newCodeAddress = ZERO_ADDRESS;
    const contractName = LogicContract.contractName;
    if (await codeChanged(LogicContract, codeAddress)) {
        console.log(`${contractName} logic code has changed`);
        newCodeAddress = await deployFunc();
        console.log(`${contractName} new logic code address ${newCodeAddress}`);
    } else {
        console.log(`${contractName} has the same logic code, no deployment needed.`);
    }
    return newCodeAddress;
}

/**
 * @dev Deploy the superfluid framework
 * @param newTestResolver Force to create a new resolver
 * @param useMocks Use mock contracts instead
 * @param nonUpgradable Deploy contracts configured to be non-upgradable
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

        const CFAv1_TYPE = web3.utils.sha3("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
        const IDAv1_TYPE = web3.utils.sha3("org.superfluid-finance.agreements.InstantDistributionAgreement.v1");

        const accounts = await web3.eth.getAccounts();

        reset = !!process.env.RESET;
        const version = process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("reset: ", reset);
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        useMocks = useMocks || process.env.USE_MOCKS;
        nonUpgradable = nonUpgradable || process.env.NON_UPGRADABLE;
        if (useMocks) console.log("**** !ATTN! USING MOCKS CONTRACTS ****");
        if (nonUpgradable) console.log("**** !ATTN! DISABLED UPGRADABILITY ****");

        const config = SuperfluidSDK.getConfig(chainId);

        if (!newTestResolver && config.resolverAddress) {
            testResolver = await TestResolver.at(config.resolverAddress);
        } else {
            testResolver = await web3tx(TestResolver.new, "TestResolver.new")();
            // make it available for the sdk for testing purpose
            process.env.TEST_RESOLVER_ADDRESS = testResolver.address;
        }
        console.log("Resolver address", testResolver.address);

        // deploy new governance contract
        let governance = await deployAndRegisterContractIf(
            TestGovernance,
            `TestGovernance.${version}`,
            async contractAddress => await codeChanged(TestGovernance, contractAddress),
            async () => {
                return await web3tx(TestGovernance.new, "TestGovernance.new")(
                    accounts[0], // rewardAddress
                    3600, // liquidationPeriod
                );
            }
        );

        // deploy new superfluid host contract
        const SuperfluidLogic = useMocks ? SuperfluidMock : Superfluid;
        let superfluid = await deployAndRegisterContractIf(
            SuperfluidLogic,
            `Superfluid.${version}`,
            async contractAddress => !await hasCode(contractAddress),
            async () => {
                let superfluidAddress;
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
                const superfluid = await Superfluid.at(superfluidAddress);
                await web3tx(superfluid.initialize, "Superfluid.initialize")(
                    governance.address
                );
                if (!nonUpgradable) {
                    if (await codeChanged(SuperfluidLogic, await superfluid.getCodeAddress())) {
                        throw new Error("Unexpected code change from fresh deployment");
                    }
                }
                return superfluid;
            }
        );

        // replace with new governance
        if ((await superfluid.getGovernance.call()) !== governance.address){
            const currentGovernance = await ISuperfluidGovernance.at(await superfluid.getGovernance.call());
            await web3tx(currentGovernance.replaceGovernance, "governance.replaceGovernance")(
                superfluid.address, governance.address
            );
        }

        // list CFA v1
        const deployCFAv1 = async () => {
            const agreement = await web3tx(
                ConstantFlowAgreementV1.new,
                "ConstantFlowAgreementV1.new")();
            console.log("New ConstantFlowAgreementV1 address", agreement.address);
            return agreement;
        };
        if (!(await superfluid.isAgreementTypeListed.call(CFAv1_TYPE))) {
            const cfa = await deployCFAv1();
            await web3tx(governance.registerAgreementClass, "Governance registers CFA")(
                superfluid.address,
                cfa.address
            );
        }

        // list IDA v1
        const deployIDAv1 = async () => {
            const agreement = await web3tx(
                InstantDistributionAgreementV1.new,
                "InstantDistributionAgreementV1.new")();
            console.log("New InstantDistributionAgreementV1 address", agreement.address);
            return agreement;
        };
        if (!(await superfluid.isAgreementTypeListed.call(IDAv1_TYPE))) {
            const ida = await deployIDAv1();
            await web3tx(governance.registerAgreementClass, "Governance registers IDA")(
                superfluid.address,
                ida.address
            );
        }

        let superfluidNewLogicAddress = ZERO_ADDRESS;
        const agreementsToUpdate = [];
        if (!nonUpgradable) {
            if (await superfluid.NON_UPGRADABLE_DEPLOYMENT.call()) {
                throw new Error("Superfluid is not upgradable");
            }

            // deploy new superfluid host logic
            superfluidNewLogicAddress = await deployNewLogicContractIfNew(
                SuperfluidLogic,
                await superfluid.getCodeAddress(),
                async () => {
                    if (!(await isProxiable(Proxiable, superfluid.address))) {
                        throw new Error("Superfluid is non-upgradable");
                    }
                    const superfluidLogic = await web3tx(SuperfluidLogic.new, "SuperfluidLogic.new")(
                        nonUpgradable
                    );
                    return superfluidLogic.address;
                }
            );

            // deploy new CFA logic
            const cfaNewLogicAddress = await deployNewLogicContractIfNew(
                ConstantFlowAgreementV1,
                await (await Proxiable.at(await superfluid.getAgreementClass.call(CFAv1_TYPE))).getCodeAddress(),
                async () => (await deployCFAv1()).address
            );
            if (cfaNewLogicAddress !== ZERO_ADDRESS) agreementsToUpdate.push(cfaNewLogicAddress);

            // deploy new IDA logic
            const idaNewLogicAddress = await deployNewLogicContractIfNew(
                InstantDistributionAgreementV1,
                await (await Proxiable.at(await superfluid.getAgreementClass.call(IDAv1_TYPE))).getCodeAddress(),
                async () => (await deployIDAv1()).address
            );
            if (idaNewLogicAddress !== ZERO_ADDRESS) agreementsToUpdate.push(idaNewLogicAddress);
        }

        // deploy new super token factory logic
        const SuperTokenFactoryLogic = useMocks ? SuperTokenFactoryMock : SuperTokenFactory;
        const superTokenFactoryNewLogicAddress = await deployNewLogicContractIfNew(
            SuperTokenFactoryLogic,
            await superfluid.getSuperTokenFactoryLogic.call(),
            async () => {
                const superTokenLogic = await web3tx(SuperTokenFactoryLogic.new, "SuperTokenFactoryLogic.new")(
                    superfluid.address
                );
                return superTokenLogic.address;
            }
        );

        await web3tx(governance.updateContracts, "superfluid.updateContracts")(
            superfluid.address,
            superfluidNewLogicAddress,
            agreementsToUpdate,
            superTokenFactoryNewLogicAddress
        );

        callback();
    } catch (err) {
        callback(err);
    }
};
