const fs = require("fs");
const util = require("util");
const getConfig = require("./libs/getConfig");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const ethers = require("ethers");
const {web3tx} = require("@decentral.ee/web3-helpers");
const deployERC1820 = require("../scripts/deploy-erc1820");

const {
    getScriptRunnerFactory: S,
    ZERO_ADDRESS,
    hasCode,
    codeChanged,
    isProxiable,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

let resetSuperfluidFramework;
let resolver;

/// @param deployFunc must return a contract object
/// @returns the newly deployed or existing loaded contract
async function deployAndRegisterContractIf(
    Contract,
    resolverKey,
    cond,
    deployFunc
) {
    let contractDeployed;
    const contractName = Contract.contractName;
    const contractAddress = await resolver.get.call(resolverKey);
    console.log(`${resolverKey} address`, contractAddress);
    if (resetSuperfluidFramework || (await cond(contractAddress))) {
        console.log(`${contractName} needs new deployment.`);
        contractDeployed = await deployFunc();
        console.log(`${resolverKey} deployed to`, contractDeployed.address);
        await web3tx(resolver.set, `Resolver set ${resolverKey}`)(
            resolverKey,
            contractDeployed.address
        );
    } else {
        console.log(`${contractName} does not need new deployment.`);
        contractDeployed = await Contract.at(contractAddress);
    }
    return contractDeployed;
}

/// @param deployFunc must return a contract address
/// @returns the address of the newly deployed contract or ZERO_ADDRESS if not deployed
async function deployContractIf(web3, Contract, cond, deployFunc) {
    let newCodeAddress = ZERO_ADDRESS;
    const contractName = Contract.contractName;
    if (await cond()) {
        console.log(`${contractName} logic code has changed`);
        newCodeAddress = await deployFunc();
        console.log(`${contractName} new logic code address ${newCodeAddress}`);
    } else {
        console.log(
            `${contractName} has the same logic code, no deployment needed.`
        );
    }
    return newCodeAddress;
}

/// @param deployFunc must return a contract address
/// @returns the address of the newly deployed contract or ZERO_ADDRESS if not deployed
async function deployContractIfCodeChanged(
    web3,
    Contract,
    codeAddress,
    deployFunc,
    codeReplacements
) {
    return deployContractIf(
        web3,
        Contract,
        async () =>
            await codeChanged(web3, Contract, codeAddress, codeReplacements),
        deployFunc
    );
}

/**
 * @dev Deploy the superfluid framework
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.newTestResolver Force to create a new resolver (overriding env: CREATE_NEW_RESOLVER)
 * @param {boolean} options.useMocks Use mock contracts instead (overriding env: USE_MOCKS)
 * @param {boolean} options.nonUpgradable Deploy contracts configured to be non-upgradable
 *                  (overriding env: NON_UPGRADABLE)
 * @param {boolean} options.appWhiteListing Deploy contracts configured to require app white listing
 *                  (overriding env: ENABLE_APP_WHITELISTING)
 * @param {boolean} options.resetSuperfluidFramework Reset the superfluid framework deployment
 *                  (overriding env: RESET_SUPERFLUID_FRAMEWORK)
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *                  (overriding env: RELEASE_VERSION)
 * @param {boolean} options.outputFile Name of file where to log addresses of newly deployed contracts
 *                  (overriding env: OUTPUT_FILE)
 * @param {boolean} options.cfaHookContract Address of the contract to be set up as CFA hooks receiver
 *                  (overriding env: CFA_HOOK_CONTRACT)
 *
 * Usage: npx truffle exec scripts/deploy-framework.js
 */

module.exports = eval(`(${S.toString()})({skipArgv: true})`)(async function (
    args,
    options = {}
) {
    console.log("======== Deploying superfluid framework ========");
    let {
        newTestResolver,
        useMocks,
        nonUpgradable,
        appWhiteListing,
        protocolReleaseVersion,
        outputFile,
        cfaHookContract
    } = options;
    resetSuperfluidFramework = options.resetSuperfluidFramework;

    resetSuperfluidFramework =
        resetSuperfluidFramework || !!process.env.RESET_SUPERFLUID_FRAMEWORK;
    console.log("reset superfluid framework: ", resetSuperfluidFramework);

    outputFile = outputFile || process.env.OUTPUT_FILE;
    console.log("output file: ", outputFile);

    cfaHookContract = cfaHookContract || process.env.CFA_HOOK_CONTRACT;
    console.log("CFA hook contract", cfaHookContract);

    // string to build a list of newly deployed contracts, written to a file if "outputFile" option set
    let output = "";

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);
    output += `NETWORK_ID=${networkId}\n`;

    const CFAv1_TYPE = web3.utils.sha3(
        "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
    );
    const IDAv1_TYPE = web3.utils.sha3(
        "org.superfluid-finance.agreements.InstantDistributionAgreement.v1"
    );

    newTestResolver = newTestResolver || !!process.env.CREATE_NEW_RESOLVER;
    useMocks = useMocks || !!process.env.USE_MOCKS;
    nonUpgradable = nonUpgradable || !!process.env.NON_UPGRADABLE;
    appWhiteListing =
        appWhiteListing ||
        config.gov_enableAppWhiteListing ||
        !!process.env.ENABLE_APP_WHITELISTING;
    if (newTestResolver) {
        console.log("**** !ATTN! CREATING NEW RESOLVER ****");
    }
    if (useMocks) {
        console.log("**** !ATTN! USING MOCKS CONTRACTS ****");
    }
    if (nonUpgradable) {
        console.log("**** !ATTN! DISABLED UPGRADABILITY ****");
    }
    if (appWhiteListing) {
        console.log("**** !ATTN! ENABLING APP WHITELISTING ****");
    }

    await deployERC1820((err) => {
        if (err) throw err;
    }, options);

    const contracts = [
        "Ownable",
        "CFAv1Forwarder",
        "IMultiSigWallet",
        "SuperfluidGovernanceBase",
        "Resolver",
        "SuperfluidLoader",
        "Superfluid",
        "SuperTokenFactory",
        "SuperTokenFactoryHelper",
        "SuperToken",
        "TestGovernance",
        "ISuperfluidGovernance",
        "UUPSProxy",
        "UUPSProxiable",
        "SlotsBitmapLibrary",
        "ConstantFlowAgreementV1",
        "InstantDistributionAgreementV1",
    ];
    const mockContracts = [
        "SuperfluidMock",
        "SuperTokenFactoryMock",
        "SuperTokenFactoryMockHelper",
        "SuperTokenMock",
    ];
    const {
        Ownable,
        IMultiSigWallet,
        CFAv1Forwarder,
        SuperfluidGovernanceBase,
        Resolver,
        SuperfluidLoader,
        Superfluid,
        SuperfluidMock,
        SuperTokenFactory,
        SuperTokenFactoryHelper,
        SuperTokenFactoryMock,
        SuperTokenFactoryMockHelper,
        SuperToken,
        SuperTokenMock,
        TestGovernance,
        ISuperfluidGovernance,
        UUPSProxy,
        UUPSProxiable,
        SlotsBitmapLibrary,
        ConstantFlowAgreementV1,
        InstantDistributionAgreementV1,
    } = await SuperfluidSDK.loadContracts({
        ...extractWeb3Options(options),
        additionalContracts: contracts.concat(useMocks ? mockContracts : []),
        contractLoader: builtTruffleContractLoader,
        networkId,
    });

    if (!newTestResolver && config.resolverAddress) {
        resolver = await Resolver.at(config.resolverAddress);
    } else {
        resolver = await web3tx(Resolver.new, "Resolver.new")();
        // make it available for the sdk for testing purpose
        process.env.RESOLVER_ADDRESS = resolver.address;
    }
    console.log("Resolver address", resolver.address);

    // deploy new governance contract
    let governanceInitializationRequired = false;
    let governance;
    if (!config.disableTestGovernance && !process.env.NO_NEW_GOVERNANCE) {
        governance = await deployAndRegisterContractIf(
            TestGovernance,
            `TestGovernance.${protocolReleaseVersion}`,
            async (contractAddress) =>
                await codeChanged(web3, TestGovernance, contractAddress),
            async () => {
                governanceInitializationRequired = true;
                const c = await web3tx(
                    TestGovernance.new,
                    "TestGovernance.new"
                )();
                output += `SUPERFLUID_GOVERNANCE=${c.address}\n`;
                return c;
            }
        );
    }

    // deploy superfluid loader
    await deployAndRegisterContractIf(
        SuperfluidLoader,
        "SuperfluidLoader-v1",
        async (contractAddress) => contractAddress === ZERO_ADDRESS,
        async () => {
            const c = await web3tx(
                SuperfluidLoader.new,
                "SuperfluidLoader.new"
            )(resolver.address);
            output += `SUPERFLUID_LOADER=${c.address}\n`;
            return c;
        }
    );

    // deploy new superfluid host contract
    const SuperfluidLogic = useMocks ? SuperfluidMock : Superfluid;
    let superfluid = await deployAndRegisterContractIf(
        SuperfluidLogic,
        `Superfluid.${protocolReleaseVersion}`,
        async (contractAddress) => !(await hasCode(web3, contractAddress)),
        async () => {
            governanceInitializationRequired = true;
            let superfluidAddress;
            const superfluidLogic = await web3tx(
                SuperfluidLogic.new,
                "SuperfluidLogic.new"
            )(nonUpgradable, appWhiteListing);
            console.log(
                `Superfluid new code address ${superfluidLogic.address}`
            );
            output += `SUPERFLUID_HOST_LOGIC=${superfluidLogic.address}\n`;
            if (!nonUpgradable) {
                const proxy = await web3tx(
                    UUPSProxy.new,
                    "Create Superfluid proxy"
                )();
                output += `SUPERFLUID_HOST_PROXY=${proxy.address}\n`;
                await web3tx(
                    proxy.initializeProxy,
                    "proxy.initializeProxy"
                )(superfluidLogic.address);
                superfluidAddress = proxy.address;
            } else {
                superfluidAddress = superfluidLogic.address;
            }
            const superfluid = await Superfluid.at(superfluidAddress);
            await web3tx(
                superfluid.initialize,
                "Superfluid.initialize"
            )(governance.address);
            if (!nonUpgradable) {
                if (
                    await codeChanged(
                        web3,
                        SuperfluidLogic,
                        await superfluid.getCodeAddress()
                    )
                ) {
                    throw new Error(
                        "Unexpected code change from fresh deployment"
                    );
                }
            }
            return superfluid;
        }
    );

    // load existing governance if needed
    if (!governance) {
        governance = await ISuperfluidGovernance.at(
            await superfluid.getGovernance.call()
        );
        console.log("Governance address", governance.address);
    }

    // initialize the new governance
    if (governanceInitializationRequired) {
        const accounts = await web3.eth.getAccounts();
        await web3tx(governance.initialize, "governance.initialize")(
            superfluid.address,
            // let rewardAddress the first account
            accounts[0],
            // liquidationPeriod
            config.liquidationPeriod,
            // patricianPeriod
            config.patricianPeriod,
            // trustedForwarders
            config.biconomyForwarder ? [config.biconomyForwarder] : []
        );
    }

    // replace with new governance
    if ((await superfluid.getGovernance.call()) !== governance.address) {
        const currentGovernance = await ISuperfluidGovernance.at(
            await superfluid.getGovernance.call()
        );
        await web3tx(
            currentGovernance.replaceGovernance,
            "governance.replaceGovernance"
        )(superfluid.address, governance.address);
    }

    // list CFA v1
    const deployCFAv1 = async () => {
        // @note Once we have the actual implementation for the hook contract,
        // we will need to deploy it and put it here instead of ZERO_ADDRESS
        const hookContractAddress = cfaHookContract || ZERO_ADDRESS;
        console.log("CFA Hook Contract Address:", hookContractAddress);

        const agreement = await web3tx(
            ConstantFlowAgreementV1.new,
            "ConstantFlowAgreementV1.new"
        )(superfluid.address, hookContractAddress);

        console.log("New ConstantFlowAgreementV1 address", agreement.address);
        output += `CFA_LOGIC=${agreement.address}\n`;
        return agreement;
    };

    if (!(await superfluid.isAgreementTypeListed.call(CFAv1_TYPE))) {
        const cfa = await deployCFAv1();
        await web3tx(
            governance.registerAgreementClass,
            "Governance registers CFA"
        )(superfluid.address, cfa.address);
    }

    // list IDA v1
    const deployIDAv1 = async () => {
        const deploySlotsBitmapLibrary = async () => {
            const slotsBmpLib = await web3tx(
                SlotsBitmapLibrary.new,
                "SlotsBitmapLibrary.new"
            )();
            output += `SLOTS_BITMAP_LIBRARY_ADDRESS=${slotsBmpLib.address}\n`;
            if (process.env.IS_HARDHAT) {
                InstantDistributionAgreementV1.link(slotsBmpLib);
            } else {
                InstantDistributionAgreementV1.link(
                    "SlotsBitmapLibrary",
                    slotsBmpLib.address
                );
            }
            return slotsBmpLib;
        };
        // small inefficiency: this may be re-deployed even if not changed
        await deploySlotsBitmapLibrary();
        const agreement = await web3tx(
            InstantDistributionAgreementV1.new,
            "InstantDistributionAgreementV1.new"
        )(superfluid.address);
        console.log(
            "New InstantDistributionAgreementV1 address",
            agreement.address
        );
        output += `IDA_LOGIC=${agreement.address}\n`;
        return agreement;
    };

    if (!(await superfluid.isAgreementTypeListed.call(IDAv1_TYPE))) {
        const ida = await deployIDAv1();
        await web3tx(
            governance.registerAgreementClass,
            "Governance registers IDA"
        )(superfluid.address, ida.address);
    } else {
        // link library in order to avoid spurious code change detections
        let slotsBitmapLibraryAddress = ZERO_ADDRESS;
        try {
            slotsBitmapLibraryAddress = await (
                await InstantDistributionAgreementV1.at(
                    await superfluid.getAgreementClass.call(IDAv1_TYPE)
                )
            ).SLOTS_BITMAP_LIBRARY_ADDRESS.call();
            if (process.env.IS_HARDHAT) {
                if (slotsBitmapLibraryAddress !== ZERO_ADDRESS) {
                    const lib = await SlotsBitmapLibrary.at(
                        slotsBitmapLibraryAddress
                    );
                    InstantDistributionAgreementV1.link(lib);
                }
            } else {
                InstantDistributionAgreementV1.link(
                    "SlotsBitmapLibrary",
                    slotsBitmapLibraryAddress
                );
            }
        } catch (e) {
            console.warn("Cannot get slotsBitmapLibrary address", e.toString());
        }
    }

    // deploy CFAv1Forwarder for test deployments
    // for other (permanent) deployments, it's not handled by this script
    if (protocolReleaseVersion === "test") {
        await deployAndRegisterContractIf(
            CFAv1Forwarder,
            "CFAv1Forwarder",
            async (contractAddress) => contractAddress === ZERO_ADDRESS,
            async () => {
                const forwarder = await CFAv1Forwarder.new(superfluid.address);
                output += `CFA_V1_FORWARDER=${forwarder.address}\n`;
                await web3tx(
                    governance.enableTrustedForwarder,
                    "Governance set CFAv1Forwarder"
                )(superfluid.address, ZERO_ADDRESS, forwarder.address);
                return forwarder;
            }
        );
    }

    let superfluidNewLogicAddress = ZERO_ADDRESS;
    const agreementsToUpdate = [];
    if (!nonUpgradable) {
        if (await superfluid.NON_UPGRADABLE_DEPLOYMENT.call()) {
            throw new Error("Superfluid is not upgradable");
        }

        // deploy new superfluid host logic
        superfluidNewLogicAddress = await deployContractIfCodeChanged(
            web3,
            SuperfluidLogic,
            await superfluid.getCodeAddress(),
            async () => {
                if (!(await isProxiable(UUPSProxiable, superfluid.address))) {
                    throw new Error("Superfluid is non-upgradable");
                }
                const superfluidLogic = await web3tx(
                    SuperfluidLogic.new,
                    "SuperfluidLogic.new"
                )(nonUpgradable, appWhiteListing);
                output += `SUPERFLUID_HOST_LOGIC=${superfluidLogic.address}\n`;
                return superfluidLogic.address;
            }
        );

        // deploy new CFA logic
        const cfaNewLogicAddress = await deployContractIfCodeChanged(
            web3,
            ConstantFlowAgreementV1,
            await (
                await UUPSProxiable.at(
                    await superfluid.getAgreementClass.call(CFAv1_TYPE)
                )
            ).getCodeAddress(),
            async () => (await deployCFAv1()).address,
            [
                // See SuperToken constructor parameter
                superfluid.address.toLowerCase().slice(2).padStart(64, "0"),
            ]
        );
        if (cfaNewLogicAddress !== ZERO_ADDRESS)
            agreementsToUpdate.push(cfaNewLogicAddress);

        // deploy new IDA logic
        const idaNewLogicAddress = await deployContractIfCodeChanged(
            web3,
            InstantDistributionAgreementV1,
            await (
                await UUPSProxiable.at(
                    await superfluid.getAgreementClass.call(IDAv1_TYPE)
                )
            ).getCodeAddress(),
            async () => (await deployIDAv1()).address,
            [
                // See SuperToken constructor parameter
                superfluid.address.toLowerCase().slice(2).padStart(64, "0"),
            ]
        );
        if (idaNewLogicAddress !== ZERO_ADDRESS)
            agreementsToUpdate.push(idaNewLogicAddress);
    }

    // deploy new super token factory logic
    const SuperTokenFactoryHelperLogic = useMocks
        ? SuperTokenFactoryMockHelper
        : SuperTokenFactoryHelper;
    const SuperTokenFactoryLogic = useMocks
        ? SuperTokenFactoryMock
        : SuperTokenFactory;
    const SuperTokenLogic = useMocks ? SuperTokenMock : SuperToken;
    const superTokenFactoryNewLogicAddress = await deployContractIf(
        web3,
        SuperTokenFactoryLogic,
        async () => {
            // check if super token factory or super token logic changed
            try {
                const factoryAddress =
                    await superfluid.getSuperTokenFactory.call();
                if (factoryAddress === ZERO_ADDRESS) return true;
                const factory = await SuperTokenFactoryLogic.at(factoryAddress);
                return (
                    (await codeChanged(
                        web3,
                        SuperTokenFactoryLogic,
                        await superfluid.getSuperTokenFactoryLogic.call()
                    )) ||
                    (await codeChanged(
                        web3,
                        SuperTokenLogic,
                        await factory.getSuperTokenLogic.call(),
                        // this replacement does not support SuperTokenMock
                        [
                            // See SuperToken constructor parameter
                            superfluid.address
                                .toLowerCase()
                                .slice(2)
                                .padStart(64, "0"),
                        ]
                    ))
                );
            } catch (e) {
                console.log(e.toString());
                // recreate contract on any errors
                return true;
            }
        },
        async () => {
            let superTokenFactoryLogic;
            const helper = await web3tx(
                SuperTokenFactoryHelperLogic.new,
                "SuperTokenFactoryHelperLogic.new"
            )();
            superTokenFactoryLogic = await web3tx(
                SuperTokenFactoryLogic.new,
                "SuperTokenFactoryLogic.new"
            )(superfluid.address, helper.address);
            output += `SUPERFLUID_SUPER_TOKEN_FACTORY_LOGIC=${superTokenFactoryLogic.address}\n`;
            return superTokenFactoryLogic.address;
        }
    );

    if (
        superfluidNewLogicAddress !== ZERO_ADDRESS ||
        agreementsToUpdate.length > 0 ||
        superTokenFactoryNewLogicAddress !== ZERO_ADDRESS
    ) {
        await sendGovernanceAction(
            {
                host: superfluid,
                contracts: {
                    Ownable,
                    IMultiSigWallet,
                    SuperfluidGovernanceBase,
                },
            },
            (gov) =>
                gov.updateContracts(
                    superfluid.address,
                    superfluidNewLogicAddress,
                    agreementsToUpdate,
                    superTokenFactoryNewLogicAddress
                )
        );
    }

    console.log("======== Superfluid framework deployed ========");

    if (process.env.RESOLVER_ADDRESS) {
        console.log(
            "=============== TEST ENVIRONMENT RESOLVER ======================"
        );
        console.log(`export RESOLVER_ADDRESS=${process.env.RESOLVER_ADDRESS}`);
    }

    if (outputFile !== undefined) {
        await util.promisify(fs.writeFile)(outputFile, output);
        console.log(
            `List of newly deployed contracts written to ${outputFile}`
        );
    }
});
