const fs = require("fs");
const util = require("util");
const getConfig = require("./libs/getConfig");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {web3tx} = require("@decentral.ee/web3-helpers");
const deployERC1820 = require("../ops-scripts/deploy-erc1820");

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
    const contractName = Contract.contractName;
    if (await cond()) {
        console.log(`${contractName} logic code has changed`);
        const newCodeAddress = await deployFunc();
        console.log(`${contractName} new logic code address ${newCodeAddress}`);
        return newCodeAddress;
    } else {
        console.log(
            `${contractName} has the same logic code, no deployment needed.`
        );
        return ZERO_ADDRESS;
    }
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
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *                  (overriding env: RELEASE_VERSION)
 * @param {string} options.outputFile Name of file where to log addresses of newly deployed contracts
 *                  (overriding env: OUTPUT_FILE)
 *
 * Usage: npx truffle exec ops-scripts/deploy-framework.js
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
    } = options;
    resetSuperfluidFramework = options.resetSuperfluidFramework;

    resetSuperfluidFramework =
        resetSuperfluidFramework || !!process.env.RESET_SUPERFLUID_FRAMEWORK;
    console.log("reset superfluid framework: ", resetSuperfluidFramework);

    outputFile = outputFile || process.env.OUTPUT_FILE;
    if (outputFile !== undefined) {
        console.log("output file: ", outputFile);
    }

    // string to build a list of newly deployed contracts, written to a file if "outputFile" option set
    let output = "";

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    const deployerAddr = (await web3.eth.getAccounts())[0];
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    console.log("deployer: ", deployerAddr);
    const config = getConfig(chainId);
    output += `NETWORK_ID=${networkId}\n`;

    const deployerInitialBalance = await web3.eth.getBalance(deployerAddr);

    const CFAv1_TYPE = web3.utils.sha3(
        "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
    );
    const IDAv1_TYPE = web3.utils.sha3(
        "org.superfluid-finance.agreements.InstantDistributionAgreement.v1"
    );
    const GDAv1_TYPE = web3.utils.sha3(
        "org.superfluid-finance.agreements.GeneralDistributionAgreement.v1"
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
        "SuperToken",
        "TestGovernance",
        "ISuperfluidGovernance",
        "UUPSProxy",
        "UUPSProxiable",
        "SlotsBitmapLibrary",
        "ConstantFlowAgreementV1",
        "InstantDistributionAgreementV1",
        "GeneralDistributionAgreementV1",
        "SuperfluidUpgradeableBeacon",
        "SuperfluidPool",
        "SuperfluidPoolDeployerLibrary",
        "ConstantOutflowNFT",
        "ConstantInflowNFT",
        "PoolAdminNFT",
        "PoolMemberNFT",
    ];
    const mockContracts = [
        "SuperfluidMock",
        "SuperTokenFactoryMock",
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
        SuperTokenFactoryMock,
        SuperToken,
        SuperTokenMock,
        TestGovernance,
        ISuperfluidGovernance,
        UUPSProxy,
        UUPSProxiable,
        SlotsBitmapLibrary,
        ConstantFlowAgreementV1,
        InstantDistributionAgreementV1,
        GeneralDistributionAgreementV1,
        SuperfluidUpgradeableBeacon,
        SuperfluidPool,
        SuperfluidPoolDeployerLibrary,
        ConstantOutflowNFT,
        ConstantInflowNFT,
        PoolAdminNFT,
        PoolMemberNFT,
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
    const superfluid = await deployAndRegisterContractIf(
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
    // this is needed later on
    const superfluidConstructorParam = superfluid.address
        .toLowerCase().slice(2).padStart(64, "0");

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
        console.log(
            `initializing governance with config: ${JSON.stringify(
                config,
                null,
                2
            )}`
        );
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
        if (config.cfaFwd !== undefined) {
            await web3tx(
                governance.enableTrustedForwarder,
                "governance.enableTrustedForwarder"
            )(superfluid.address, ZERO_ADDRESS, config.cfaFwd);
        }
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
        const agreement = await web3tx(
            ConstantFlowAgreementV1.new,
            "ConstantFlowAgreementV1.new"
        )(superfluid.address, ZERO_ADDRESS);

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

    /**
     * This function:
     * deploys an external library
     * links a contract artifact to the deployed external library (in two ways depending on if hardhat or truffle env)
     * returns the deployed external library
     * @param {*} externalLibraryArtifact artifact of the external library
     * @param {*} externalLibraryName name of the external library
     * @param {*} outputName the output name
     * @param {*} contract the contract artifact to link to the external library
     * @returns
     */
    const deployExternalLibraryAndLink = async (
        externalLibraryArtifact,
        externalLibraryName,
        outputName,
        contract
    ) => {
        const externalLibrary = await web3tx(
            externalLibraryArtifact.new,
            `${externalLibraryName}.new`
        )();
        output += `${outputName}=${externalLibrary.address}\n`;
        if (process.env.IS_HARDHAT) {
            contract.link(externalLibrary);
        } else {
            contract.link(externalLibraryName, externalLibrary.address);
        }
        console.log(externalLibraryName, "address", externalLibrary.address);
        return externalLibrary;
    };

    let slotsBitmapLibraryAddress = ZERO_ADDRESS;
    // list IDA v1
    const deployIDAv1 = async () => {
        // small inefficiency: this may be re-deployed even if not changed
        // deploySlotsBitmapLibrary
        const slotsBitmapLibrary = await deployExternalLibraryAndLink(
            SlotsBitmapLibrary,
            "SlotsBitmapLibrary",
            "SLOTS_BITMAP_LIBRARY_ADDRESS",
            InstantDistributionAgreementV1
        );
        slotsBitmapLibraryAddress = slotsBitmapLibrary.address;
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
        // NOTE that we are reusing the existing deployed external library
        // here as an optimization, this assumes that we do not change the
        // library code.
        // link library in order to avoid spurious code change detections
        try {
            const IDAv1 = await InstantDistributionAgreementV1.at(
                await superfluid.getAgreementClass.call(IDAv1_TYPE)
            );
            slotsBitmapLibraryAddress =
                await IDAv1.SLOTS_BITMAP_LIBRARY_ADDRESS.call();
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

    // @note GDA deployment is commented out until we plan on releasing it
    const deployGDAv1 = async () => {
        try {
            // deploy and link SuperfluidPoolDeployerLibrary
            await deployExternalLibraryAndLink(
                SuperfluidPoolDeployerLibrary,
                "SuperfluidPoolDeployerLibrary",
                "SUPER_TOKEN_POOL_DEPLOYER_LIBRARY_ADDRESS",
                GeneralDistributionAgreementV1
            );

            if (process.env.IS_HARDHAT) {
                if (slotsBitmapLibraryAddress !== ZERO_ADDRESS) {
                    const lib = await SlotsBitmapLibrary.at(
                        slotsBitmapLibraryAddress
                    );
                    GeneralDistributionAgreementV1.link(lib);
                }
            } else {
                GeneralDistributionAgreementV1.link(
                    "SlotsBitmapLibrary",
                    slotsBitmapLibraryAddress
                );
            }
        } catch (err) {
            console.error(err);
        }
        const agreement = await web3tx(
            GeneralDistributionAgreementV1.new,
            "GeneralDistributionAgreementV1.new"
        )(superfluid.address);

        console.log(
            "New GeneralDistributionAgreementV1 address",
            agreement.address
        );
        output += `GDA_LOGIC=${agreement.address}\n`;
        return agreement;
    };

    if (!(await superfluid.isAgreementTypeListed.call(GDAv1_TYPE))) {
        const gda = await deployGDAv1();
        await web3tx(
            governance.registerAgreementClass,
            "Governance registers GDA"
        )(superfluid.address, gda.address);
    } else {
        // NOTE that we are reusing the existing deployed external library
        // here as an optimization, this assumes that we do not change the
        // library code.
        // link library in order to avoid spurious code change detections
        try {
            const GDAv1 = await GeneralDistributionAgreementV1.at(
                await superfluid.getAgreementClass.call(GDAv1_TYPE)
            );
            slotsBitmapLibraryAddress =
                await GDAv1.SLOTS_BITMAP_LIBRARY_ADDRESS.call();
            let superfluidPoolDeployerLibraryAddress =
                await GDAv1.SUPERFLUID_POOL_DEPLOYER_ADDRESS.call();
            if (process.env.IS_HARDHAT) {
                if (slotsBitmapLibraryAddress !== ZERO_ADDRESS) {
                    const lib = await SlotsBitmapLibrary.at(
                        slotsBitmapLibraryAddress
                    );
                    GeneralDistributionAgreementV1.link(lib);
                }
                if (superfluidPoolDeployerLibraryAddress !== ZERO_ADDRESS) {
                    const lib = await SuperfluidPoolDeployerLibrary.at(
                        superfluidPoolDeployerLibraryAddress
                    );
                    GeneralDistributionAgreementV1.link(lib);
                }
            } else {
                GeneralDistributionAgreementV1.link(
                    "SlotsBitmapLibrary",
                    slotsBitmapLibraryAddress
                );
                GeneralDistributionAgreementV1.link(
                    "SuperfluidPoolDeployerLibrary",
                    superfluidPoolDeployerLibraryAddress
                );
            }
        } catch (e) {
            console.warn("Cannot get slotsBitmapLibrary address", e.toString());
        }
    }
    // @note GDA deployment is commented out until we plan on releasing it

    if (protocolReleaseVersion === "test") {
        // deploy CFAv1Forwarder for test deployments
        // for other (permanent) deployments, it's not handled by this script
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
                superfluidConstructorParam,
                ZERO_ADDRESS.toLowerCase().slice(2).padStart(64, "0"),
            ]
        );
        if (cfaNewLogicAddress !== ZERO_ADDRESS) {
            agreementsToUpdate.push(cfaNewLogicAddress);
        }
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
                superfluidConstructorParam,
            ]
        );
        if (idaNewLogicAddress !== ZERO_ADDRESS) {
            agreementsToUpdate.push(idaNewLogicAddress);
        }
        // @note commented out: deploy new GDA logic
        const gdaNewLogicAddress = await deployContractIfCodeChanged(
            web3,
            GeneralDistributionAgreementV1,
            await (
                await UUPSProxiable.at(
                    await superfluid.getAgreementClass.call(GDAv1_TYPE)
                )
            ).getCodeAddress(),
            async () => (await deployGDAv1()).address,
            [
                // See SuperToken constructor parameter
                superfluidConstructorParam,
            ]
        );
        if (gdaNewLogicAddress !== ZERO_ADDRESS) {
            agreementsToUpdate.push(gdaNewLogicAddress);
        }
        // @note GDA deployment is commented out until we plan on releasing it
    }

    // deploy new super token factory logic (depends on SuperToken logic, which links to nft deployer library)
    const SuperTokenFactoryLogic = useMocks
        ? SuperTokenFactoryMock
        : SuperTokenFactory;

    const SuperTokenLogic = useMocks ? SuperTokenMock : SuperToken;

    const factoryAddress = await superfluid.getSuperTokenFactory.call();

    let constantOutflowNFTLogicChanged = false;
    let constantInflowNFTLogicChanged = false;
    let poolAdminNFTLogicChanged = false;
    let poolMemberNFTLogicChanged = false;

    const deployNFTContract = async (artifact, nftType, nftTypeCaps, args) => {
        const nftLogic = await web3tx(artifact.new, `${nftType}.new`)(...args);
        console.log(`${nftType} Logic address`, nftLogic.address);
        output += `${nftTypeCaps}=${nftLogic.address}\n`;

        await nftLogic.castrate();

        return nftLogic;
    };

    const superTokenFactoryNewLogicAddress = await deployContractIf(
        web3,
        SuperTokenFactoryLogic,
        async () => {
            console.log(
                "checking if SuperTokenFactory needs to be redeployed..."
            );
            // check if super token factory or super token logic changed
            try {
                if (factoryAddress === ZERO_ADDRESS) return true;
                const factory = await SuperTokenFactoryLogic.at(factoryAddress);
                const superTokenLogicAddress =
                    await factory.getSuperTokenLogic.call();
                const superTokenLogic = await SuperTokenLogic.at(
                    superTokenLogicAddress
                );
                const constantOutflowNFTAddress =
                    await superTokenLogic.CONSTANT_OUTFLOW_NFT();
                const constantInflowNFTAddress =
                    await superTokenLogic.CONSTANT_INFLOW_NFT();
                console.log("   superTokenLogic.POOL_ADMIN_NFT()");
                const poolAdminNFTAddress =
                    await superTokenLogic.POOL_ADMIN_NFT();
                console.log("   superTokenLogic.POOL_MEMBER_NFT()");
                const poolMemberNFTAddress =
                    await superTokenLogic.POOL_MEMBER_NFT();

                const constantOutflowNFTContract = ConstantOutflowNFT.at(
                    constantOutflowNFTAddress
                );
                const constantInflowNFTContract = ConstantInflowNFT.at(
                    constantInflowNFTAddress
                );
                const poolAdminNFTContract =
                    PoolAdminNFT.at(poolAdminNFTAddress);
                const poolMemberNFTContract =
                    PoolMemberNFT.at(poolMemberNFTAddress);

                const constantInflowNFTParam = constantInflowNFTAddress
                    .toLowerCase().slice(2).padStart(64, "0");
                const constantOutflowNFTParam = constantOutflowNFTAddress
                    .toLowerCase().slice(2).padStart(64, "0");
                const cfaParam = (await superfluid.getAgreementClass.call(CFAv1_TYPE))
                    .toLowerCase().slice(2).padStart(64, "0");

                constantOutflowNFTLogicChanged = await codeChanged(
                    web3,
                    ConstantOutflowNFT,
                    await (
                        await UUPSProxiable.at(constantOutflowNFTAddress)
                    ).getCodeAddress(),
                    [superfluidConstructorParam, constantInflowNFTParam, cfaParam]
                );
                console.log("   constantOutflowNFTLogicChanged:", constantOutflowNFTLogicChanged);

                constantInflowNFTLogicChanged = await codeChanged(
                    web3,
                    ConstantInflowNFT,
                    await (
                        await UUPSProxiable.at(constantInflowNFTAddress)
                    ).getCodeAddress(),
                    [superfluidConstructorParam, constantOutflowNFTParam, cfaParam]
                );
                console.log("   constantInflowNFTLogicChanged:", constantInflowNFTLogicChanged);

                poolAdminNFTLogicChanged = await codeChanged(
                    web3,
                    PoolAdminNFT,
                    await poolAdminNFTContract.getCodeAddress(),
                    [superfluidConstructorParam]
                );
                console.log("   poolAdminNFTLogicChanged:", poolAdminNFTLogicChanged);

                poolMemberNFTLogicChanged = await codeChanged(
                    web3,
                    PoolMemberNFT,
                    await poolMemberNFTContract.getCodeAddress(),
                    [superfluidConstructorParam]
                );
                console.log("   poolMemberNFTLogicChanged:", poolMemberNFTLogicChanged);

                const superTokenFactoryCodeChanged = await codeChanged(
                    web3,
                    SuperTokenFactoryLogic,
                    await superfluid.getSuperTokenFactoryLogic.call(),
                    [superfluidConstructorParam]
                );
                console.log("   superTokenFactoryCodeChanged:", superTokenFactoryCodeChanged);

                const superTokenLogicCodeChanged = await codeChanged(
                    web3,
                    SuperTokenLogic,
                    await factory.getSuperTokenLogic.call(),
                    // this replacement does not support SuperTokenMock
                    [
                        // See SuperToken constructor parameter
                        superfluidConstructorParam,
                        constantOutflowNFTParam,
                        constantInflowNFTParam,
                    ]
                );
                console.log("   superTokenLogicCodeChanged:", superTokenLogicCodeChanged);
                return (
                    // check if super token factory logic has changed
                    // or super token logic has changed
                    // or constant outflow nft logic has changed
                    // or constant inflow nft logic has changed
                    superTokenFactoryCodeChanged ||
                    superTokenLogicCodeChanged ||
                    constantOutflowNFTLogicChanged ||
                    constantInflowNFTLogicChanged ||
                    poolAdminNFTLogicChanged ||
                    poolMemberNFTLogicChanged
                );
            } catch (e) {
                console.log(
                    `   re-deploying SuperTokenFactory because checks didn't pass ${e.toString()}`
                );
                // recreate contract on any errors
                return true;
            }
        },
        async () => {
            let superTokenFactoryLogic;

            // @note this will either be freshly created proxies on the very first bootstrapping per network
            // OR it will be the canonical proxy set on the SuperToken
            let cofNFTProxyAddress = ZERO_ADDRESS;
            let cifNFTProxyAddress = ZERO_ADDRESS;
            let cofNFTLogicAddress = ZERO_ADDRESS;
            let cifNFTLogicAddress = ZERO_ADDRESS;
            let poolAdminNFTProxyAddress = ZERO_ADDRESS;
            let poolAdminNFTLogicAddress = ZERO_ADDRESS;
            let poolMemberNFTProxyAddress = ZERO_ADDRESS;
            let poolMemberNFTLogicAddress = ZERO_ADDRESS;

            // try to get NFT proxy addresses from canonical Super Token logic
            if (factoryAddress !== ZERO_ADDRESS) {
                try {
                    const factory = await SuperTokenFactoryLogic.at(
                        factoryAddress
                    );
                    console.log("   factory.getSuperTokenLogic.call()");
                    const superTokenLogicAddress =
                        await factory.getSuperTokenLogic.call();
                    const superTokenLogic = await SuperTokenLogic.at(
                        superTokenLogicAddress
                    );
                    // Flow NFTs
                    cofNFTProxyAddress =
                        await superTokenLogic.CONSTANT_OUTFLOW_NFT.call();
                    cifNFTProxyAddress =
                        await superTokenLogic.CONSTANT_INFLOW_NFT.call();
                    cofNFTLogicAddress = await (
                        await UUPSProxiable.at(cofNFTProxyAddress)
                    ).getCodeAddress();
                    cifNFTLogicAddress = await (
                        await UUPSProxiable.at(cifNFTProxyAddress)
                    ).getCodeAddress();

                    // Pool NFTs
                    poolAdminNFTProxyAddress =
                        await superTokenLogic.POOL_ADMIN_NFT.call();
                    poolMemberNFTProxyAddress =
                        await superTokenLogic.POOL_MEMBER_NFT.call();
                    poolAdminNFTLogicAddress = await (
                        await UUPSProxiable.at(poolAdminNFTProxyAddress)
                    ).getCodeAddress();
                    poolMemberNFTLogicAddress = await (
                        await UUPSProxiable.at(poolMemberNFTProxyAddress)
                    ).getCodeAddress();
                } catch (err) {
                    console.error("Unable to get nft proxy addresses");
                }
            }

            // if the super token logic does not have the proxies, we must deploy
            // new nft logic and proxies.
            if (
                cofNFTProxyAddress === ZERO_ADDRESS ||
                cifNFTProxyAddress === ZERO_ADDRESS ||
                poolAdminNFTProxyAddress === ZERO_ADDRESS ||
                poolMemberNFTProxyAddress === ZERO_ADDRESS
            ) {
                if (
                    cofNFTProxyAddress === ZERO_ADDRESS ||
                    cifNFTProxyAddress === ZERO_ADDRESS
                ) {
                    const constantOutflowNFTProxy = await web3tx(
                        UUPSProxy.new,
                        `Create ConstantOutflowNFT proxy`
                    )();
                    console.log(
                        "ConstantOutflowNFT Proxy address",
                        constantOutflowNFTProxy.address
                    );
                    output += `CONSTANT_OUTFLOW_NFT_PROXY=${constantOutflowNFTProxy.address}\n`;

                    const constantInflowNFTProxy = await web3tx(
                        UUPSProxy.new,
                        `Create ConstantInflowNFT proxy`
                    )();
                    console.log(
                        "ConstantInflowNFT Proxy address",
                        constantInflowNFTProxy.address
                    );
                    output += `CONSTANT_INFLOW_NFT_PROXY=${constantInflowNFTProxy.address}\n`;

                    const constantOutflowNFTLogic = await deployNFTContract(
                        ConstantOutflowNFT,
                        "ConstantOutflowNFT",
                        "CONSTANT_OUTFLOW_NFT",
                        [superfluid.address, constantInflowNFTProxy.address]
                    );
                    const constantInflowNFTLogic = await deployNFTContract(
                        ConstantInflowNFT,
                        "ConstantInflowNFT",
                        "CONSTANT_INFLOW_NFT",
                        [superfluid.address, constantOutflowNFTProxy.address]
                    );

                    // set the nft logic addresses (to be consumed by the super token factory logic constructor)
                    cofNFTLogicAddress = constantOutflowNFTLogic.address;
                    cifNFTLogicAddress = constantInflowNFTLogic.address;

                    // initialize the nft proxy with the nft logic
                    await constantOutflowNFTProxy.initializeProxy(
                        constantOutflowNFTLogic.address
                    );
                    await constantInflowNFTProxy.initializeProxy(
                        constantInflowNFTLogic.address
                    );
                    const constantOutflowNFT = await ConstantOutflowNFT.at(
                        constantOutflowNFTProxy.address
                    );
                    const constantInflowNFT = await ConstantInflowNFT.at(
                        constantInflowNFTProxy.address
                    );

                    // initialize the proxy contracts with the nft names
                    await constantOutflowNFT.initialize(
                        "Constant Outflow NFT",
                        "COF"
                    );
                    await constantInflowNFT.initialize(
                        "Constant Inflow NFT",
                        "CIF"
                    );

                    // set the nft proxy addresses (to be consumed by the super token logic constructor)
                    cofNFTProxyAddress = constantOutflowNFTProxy.address;
                    cifNFTProxyAddress = constantInflowNFTProxy.address;
                }
                if (
                    poolAdminNFTProxyAddress === ZERO_ADDRESS ||
                    poolMemberNFTProxyAddress === ZERO_ADDRESS
                ) {
                    const poolAdminNFTProxy = await web3tx(
                        UUPSProxy.new,
                        `Create PoolAdminNFT proxy`
                    )();
                    console.log(
                        "PoolAdminNFT Proxy address",
                        poolAdminNFTProxy.address
                    );
                    output += `POOL_ADMIN_NFT_PROXY=${poolAdminNFTProxy.address}\n`;

                    const poolMemberNFTProxy = await web3tx(
                        UUPSProxy.new,
                        `Create PoolMemberNFT proxy`
                    )();
                    console.log(
                        "PoolMemberNFT Proxy address",
                        poolMemberNFTProxy.address
                    );
                    output += `POOL_MEMBER_NFT_PROXY=${poolMemberNFTProxy.address}\n`;

                    const poolAdminNFTLogic = await deployNFTContract(
                        PoolAdminNFT,
                        "PoolAdminNFT",
                        "POOL_ADMIN_NFT",
                        [superfluid.address]
                    );
                    const poolMemberNFTLogic = await deployNFTContract(
                        PoolMemberNFT,
                        "PoolMemberNFT",
                        "POOL_MEMBER_NFT",
                        [superfluid.address]
                    );

                    // set the nft logic addresses (to be consumed by the super token factory logic constructor)
                    poolAdminNFTLogicAddress = poolAdminNFTLogic.address;
                    poolMemberNFTLogicAddress = poolMemberNFTLogic.address;

                    // initialize the nft proxy with the nft logic
                    await poolAdminNFTProxy.initializeProxy(
                        poolAdminNFTLogic.address
                    );

                    await poolMemberNFTProxy.initializeProxy(
                        poolMemberNFTLogic.address
                    );

                    const poolAdminNFT = await PoolAdminNFT.at(
                        poolAdminNFTProxy.address
                    );

                    const poolMemberNFT = await PoolMemberNFT.at(
                        poolMemberNFTProxy.address
                    );

                    // initialize the proxy contracts with the nft names
                    await poolAdminNFT.initialize("Pool Admin NFT", "PA");
                    await poolMemberNFT.initialize("Pool Member NFT", "PM");

                    // set the nft proxy addresses (to be consumed by the super token logic constructor)
                    poolAdminNFTProxyAddress = poolAdminNFTProxy.address;
                    poolMemberNFTProxyAddress = poolMemberNFTProxy.address;
                }
            } else {
                // nft proxies already exist
                await deployContractIf(
                    web3,
                    ConstantOutflowNFT,
                    async () => {
                        return constantOutflowNFTLogicChanged;
                    },
                    async () => {
                        const cofNFTLogic = await deployNFTContract(
                            ConstantOutflowNFT,
                            "ConstantOutflowNFT",
                            "CONSTANT_OUTFLOW_NFT",
                            [superfluid.address, constantInflowNFTProxy.address]
                        );
                        // @note we set the cofNFTLogicAddress to be passed to SuperTokenFactoryLogic here
                        cofNFTLogicAddress = cofNFTLogic.address;

                        return cofNFTLogic.address;
                    }
                );
                await deployContractIf(
                    web3,
                    ConstantInflowNFT,
                    async () => {
                        return constantInflowNFTLogicChanged;
                    },
                    async () => {
                        const cifNFTLogic = await deployNFTContract(
                            ConstantInflowNFT,
                            "ConstantInflowNFT",
                            "CONSTANT_INFLOW_NFT",
                            [
                                superfluid.address,
                                constantOutflowNFTProxy.address,
                            ]
                        );
                        // @note we set the cifNFTLogicAddress to be passed to SuperTokenFactoryLogic here
                        cifNFTLogicAddress = cifNFTLogic.address;
                        return cifNFTLogic.address;
                    }
                );
                await deployContractIf(
                    web3,
                    PoolAdminNFT,
                    async () => {
                        return poolAdminNFTLogicChanged;
                    },
                    async () => {
                        const poolAdminNFTLogic = await deployNFTContract(
                            PoolAdminNFT,
                            "PoolAdminNFT",
                            "POOL_ADMIN_NFT",
                            [superfluid.address]
                        );
                        // @note we set the poolAdminNFTLogicAddress to be passed to SuperTokenFactoryLogic here
                        poolAdminNFTLogicAddress = poolAdminNFTLogic.address;
                        return poolAdminNFTLogic.address;
                    }
                );
                await deployContractIf(
                    web3,
                    PoolMemberNFT,
                    async () => {
                        return poolMemberNFTLogicChanged;
                    },
                    async () => {
                        const poolMemberNFTLogic = await deployNFTContract(
                            PoolMemberNFT,
                            "PoolMemberNFT",
                            "POOL_MEMBER_NFT",
                            [superfluid.address]
                        );
                        // @note we set the poolMemberNFTLogicAddress to be passed to SuperTokenFactoryLogic here
                        poolMemberNFTLogicAddress = poolMemberNFTLogic.address;
                        return poolMemberNFTLogic.address;
                    }
                );
            }

            // deploy super token logic contract
            // it now takes the nft logic contracts as parameters
            const superTokenLogic = useMocks
                ? await web3tx(SuperTokenLogic.new, "SuperTokenLogic.new")(
                      superfluid.address,
                      0,
                      cofNFTProxyAddress,
                      cifNFTProxyAddress,
                      poolAdminNFTProxyAddress,
                      poolMemberNFTProxyAddress
                  )
                : await web3tx(SuperTokenLogic.new, "SuperTokenLogic.new")(
                      superfluid.address,
                      cofNFTProxyAddress,
                      cifNFTProxyAddress,
                      poolAdminNFTProxyAddress,
                      poolMemberNFTProxyAddress
                  );

            console.log(
                `SuperToken new logic code address ${superTokenLogic.address}`
            );
            output += `SUPERFLUID_SUPER_TOKEN_LOGIC=${superTokenLogic.address}\n`;

            superTokenFactoryLogic = await web3tx(
                SuperTokenFactoryLogic.new,
                "SuperTokenFactoryLogic.new"
            )(
                superfluid.address,
                superTokenLogic.address,
                cofNFTLogicAddress,
                cifNFTLogicAddress,
                poolAdminNFTLogicAddress,
                poolMemberNFTLogicAddress
            );
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

    const gdaV1Contract = await GeneralDistributionAgreementV1.at(
        await superfluid.getAgreementClass.call(GDAv1_TYPE)
    );
    const superfluidPoolBeaconAddress =
        await gdaV1Contract.superfluidPoolBeacon();

    const getPoolLogicAddress = async () => {
        if (superfluidPoolBeaconAddress === ZERO_ADDRESS) {
            return ZERO_ADDRESS;
        }

        try {
            return await (
                await SuperfluidUpgradeableBeacon.at(
                    superfluidPoolBeaconAddress
                )
            ).implementation();
        } catch (e) {
            return ZERO_ADDRESS;
        }
    };
    const superfluidPoolLogicAddress = await deployContractIfCodeChanged(
        web3,
        SuperfluidPool,
        await getPoolLogicAddress(),
        async () => {
            // Deploy new SuperfluidPool logic contract
            const superfluidPoolLogic = await web3tx(
                SuperfluidPool.new,
                "SuperfluidPool.new"
            )(gdaV1Contract.address);
            await superfluidPoolLogic.castrate();
            console.log(
                "New SuperfluidPoolLogic address",
                superfluidPoolLogic.address
            );
            output += `SUPERFLUID_POOL_LOGIC=${superfluidPoolLogic.address}\n`;

            return superfluidPoolLogic.address;
        },
        [
            // See SuperToken constructor parameter
            gdaV1Contract.address.toLowerCase().slice(2).padStart(64, "0"),
        ]
    );

    // if beacon doesn't exist, we deploy a new one
    if (superfluidPoolBeaconAddress === ZERO_ADDRESS) {
        console.log(
            "SuperfluidPool Beacon doesn't exist, creating a new one..."
        );
        const superfluidPoolBeaconContract = await web3tx(
            SuperfluidUpgradeableBeacon.new,
            "SuperfluidUpgradeableBeacon.new"
        )(superfluidPoolLogicAddress);
        console.log(
            "New SuperfluidPoolBeacon address",
            superfluidPoolBeaconContract.address
        );
        output += `SUPERFLUID_POOL_BEACON=${superfluidPoolBeaconContract.address}\n`;
        console.log("Initializing GDA w/ beacon contract...");
        await gdaV1Contract.initialize(superfluidPoolBeaconContract.address);
    } else {
        console.log("Superfluid Pool Beacon exists...");
        // if the beacon exists AND we deployed a new SuperfluidPool logic contract
        if (superfluidPoolLogicAddress !== ZERO_ADDRESS) {
            console.log(
                "superfluidPoolLogicAddress updated, upgrading logic contract..."
            );
            // update beacon implementation
            const superfluidPoolBeacon = await SuperfluidUpgradeableBeacon.at(
                superfluidPoolBeaconAddress
            );
            await superfluidPoolBeacon.upgradeTo(superfluidPoolLogicAddress);
        }
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

    const deployerFinalBalance = await web3.eth.getBalance(deployerAddr);
    const consumed = web3.utils.fromWei(
        (new web3.utils.BN(deployerInitialBalance)).sub(new web3.utils.BN(deployerFinalBalance)));
    console.log(`consumed native coins: ${consumed}`);
});
