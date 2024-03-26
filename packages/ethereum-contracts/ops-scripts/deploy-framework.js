const fs = require("fs");
const util = require("util");
const { execSync } = require('child_process');
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
    setResolver,
    versionStringToPseudoAddress,
    pseudoAddressToVersionString,
    getGasConfig,
} = require("./libs/common");

let resetSuperfluidFramework;
let resolver;
let sfObjForGovAndResolver;

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
        await setResolver(sfObjForGovAndResolver, resolverKey, contractDeployed.address);
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

// helper function: encode an address as word
function ap(addr) {
    return addr.toLowerCase().slice(2).padStart(64, "0");
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
 * @param {boolean} options.newSuperfluidLoader Deploy a new superfluid loader contract
 *                  (overriding env: NEW_SUPERFLUID_LOADER)
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
        newSuperfluidLoader,
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
    if (config.isTestnet) {
        output += "IS_TESTNET=1\n";
    }
    output += `NETWORK_ID=${networkId}\n`;

    const gitRevision = execSync('git rev-parse HEAD').toString().slice(0,16).trim();
    const packageVersion = require('../package.json').version;
    const versionString = `${packageVersion}-${gitRevision}`;

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
    newSuperfluidLoader = newSuperfluidLoader || !!process.env.NEW_SUPERFLUID_LOADER;

    console.log("app whitelisting enabled:", appWhiteListing);
    if (newTestResolver) {
        console.log("**** !ATTN! CREATING NEW RESOLVER ****");
    }
    if (useMocks) {
        console.log("**** !ATTN! USING MOCKS CONTRACTS ****");
    }
    if (nonUpgradable) {
        console.log("**** !ATTN! DISABLED UPGRADABILITY ****");
    }
    if (newSuperfluidLoader) {
        console.log("**** !ATTN! DEPLOYING NEW SUPERFLUID LOADER ****");
    }

    await deployERC1820((err) => {
        if (err) throw err;
    }, options);

    const contracts = [
        "Ownable",
        "CFAv1Forwarder",
        "IDAv1Forwarder",
        "GDAv1Forwarder",
        "IMultiSigWallet",
        "ISafe",
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
        "SuperfluidPoolPlaceholder",
        "SuperfluidPoolDeployerLibrary",
        "ConstantOutflowNFT",
        "ConstantInflowNFT",
        "PoolAdminNFT",
        "PoolMemberNFT",
        "IAccessControlEnumerable",
    ];
    const mockContracts = [
        "SuperfluidMock",
        "SuperTokenFactoryMock",
        "SuperTokenMock",
    ];
    const {
        Ownable,
        IMultiSigWallet,
        ISafe,
        CFAv1Forwarder,
        IDAv1Forwarder,
        GDAv1Forwarder,
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
        SuperfluidPoolPlaceholder,
        SuperfluidPoolDeployerLibrary,
        ConstantOutflowNFT,
        ConstantInflowNFT,
        PoolAdminNFT,
        PoolMemberNFT,
        IAccessControlEnumerable,
    } = await SuperfluidSDK.loadContracts({
        ...extractWeb3Options(options),
        additionalContracts: contracts.concat(useMocks ? mockContracts : []),
        contractLoader: builtTruffleContractLoader,
        networkId,
        gasConfig: getGasConfig(networkId),
    });

    if (!newTestResolver && config.resolverAddress) {
        resolver = await Resolver.at(config.resolverAddress);
    } else {
        resolver = await web3tx(Resolver.new, "Resolver.new")();
        // make it available for the sdk for testing purpose
        process.env.RESOLVER_ADDRESS = resolver.address;
    }
    console.log("Resolver address", resolver.address);

    sfObjForGovAndResolver = {
        contracts: {
            Resolver,
            Ownable,
            IMultiSigWallet,
            ISafe,
            IAccessControlEnumerable,
            SuperfluidGovernanceBase
        },
        resolver: {
            address: resolver.address
        }
    };

    const previousVersionString = pseudoAddressToVersionString(
        await resolver.get(`versionString.${protocolReleaseVersion}`)
    );
    console.log(`previous versionString: ${previousVersionString}`);
    console.log(`new versionString:      ${versionString}`);

    // =========== BOOTSTRAPPING (initial deployment) ===========

    // deploy superfluid loader
    await deployAndRegisterContractIf(
        SuperfluidLoader,
        "SuperfluidLoader-v1",
        async (contractAddress) => newSuperfluidLoader === true || contractAddress === ZERO_ADDRESS,
        async () => {
            const c = await web3tx(
                SuperfluidLoader.new,
                "SuperfluidLoader.new"
            )(resolver.address);
            output += `SUPERFLUID_LOADER=${c.address}\n`;
            return c;
        }
    );

    // deploy new TestGovernance contract
    // (only on testnets, devnets and initial mainnet deployment)
    let testGovernanceInitRequired = false;
    let governance;
    if (!config.disableTestGovernance && !process.env.NO_NEW_GOVERNANCE) {
        const prevGovAddr = await resolver.get.call(`TestGovernance.${protocolReleaseVersion}`);
        if (resetSuperfluidFramework || await codeChanged(web3, TestGovernance, prevGovAddr)) {
            console.log(`TestGovernance needs new deployment.`);
            const c = await web3tx(TestGovernance.new,"TestGovernance.new")();
            governance = await TestGovernance.at(c.address);
            testGovernanceInitRequired = true;
            output += `SUPERFLUID_GOVERNANCE=${c.address}\n`;
        } else {
            governance = await TestGovernance.at(prevGovAddr);
        }
        // defer resolver update to after the initialization
        // this avoids testnet bricking in case script execution is interrupted
    }

    // deploy new superfluid host contract
    const SuperfluidLogic = useMocks ? SuperfluidMock : Superfluid;
    const superfluid = await deployAndRegisterContractIf(
        SuperfluidLogic,
        `Superfluid.${protocolReleaseVersion}`,
        async (contractAddress) => !(await hasCode(web3, contractAddress)),
        async () => {
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

    // helper objects needed later on
    const superfluidConstructorParam = superfluid.address
        .toLowerCase()
        .slice(2)
        .padStart(64, "0");

    sfObjForGovAndResolver.host = superfluid;

    // load existing governance if needed
    if (!governance) {
        governance = await ISuperfluidGovernance.at(
            await superfluid.getGovernance.call()
        );
        console.log("Governance address", governance.address);
    }

    // initialize the new TestGovernance
    if (testGovernanceInitRequired) {
        const accounts = await web3.eth.getAccounts();
        const trustedForwarders = [];
        if (config.trustedForwarders) {
            trustedForwarders.push(...config.trustedForwarders);
        }
        if (config.cfaFwd) {
            trustedForwarders.push(config.cfaFwd);
        }
        if (config.gdaFwd) {
            trustedForwarders.push(config.gdaFwd);
        }
        console.log(`initializing TestGovernance with config: ${JSON.stringify({
            liquidationPeriod: config.liquidationPeriod,
            patricianPeriod: config.patricityPeriod,
            trustedForwarders
        }, null, 2)}`);

        await web3tx(governance.initialize, "governance.initialize")(
            superfluid.address,
            // let rewardAddress the first account
            accounts[0],
            // liquidationPeriod
            config.liquidationPeriod,
            // patricianPeriod
            config.patricianPeriod,
            // trustedForwarders
            trustedForwarders
        );

        // update the resolver
        await setResolver(
            sfObjForGovAndResolver,
            `TestGovernance.${protocolReleaseVersion}`,
            governance.address
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
        const agreement = await web3tx(
            ConstantFlowAgreementV1.new,
            "ConstantFlowAgreementV1.new"
        )(superfluid.address);

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
        contract,
        allowFailure = false
    ) => {
        try {
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
        } catch (err) {
            console.warn("Error: ", err);
            if (!allowFailure) {
                throw err;
            }
        }
    };

    let slotsBitmapLibraryAddress = ZERO_ADDRESS;
    // list IDA v1
    const deployIDAv1 = async () => {
        // small inefficiency: this may be re-deployed even if not changed
        // deploySlotsBitmapLibrary
        const slotsBitmapLibrary = await deployExternalLibraryAndLink(
            SlotsBitmapLibrary,
            "SlotsBitmapLibrary",
            "SLOTS_BITMAP_LIBRARY",
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

    let gdaIsLinked = false;
    const deployGDAv1 = async (superfluidPoolBeaconAddr) => {
        if (!gdaIsLinked) {
            await deployExternalLibraryAndLink(
                SuperfluidPoolDeployerLibrary,
                "SuperfluidPoolDeployerLibrary",
                "SUPERFLUID_POOL_DEPLOYER_LIBRARY",
                GeneralDistributionAgreementV1,
                protocolReleaseVersion === "test" ? true : false
            );

            if (process.env.IS_HARDHAT) {
                // this fails in test case deployment.test.js:ops-scripts/deploy-super-token.js
                // where deploy-framework is invoked twice, the second time failing because
                // hardhat claims the library is already linked. Thus we try/catch here.
                try {
                    if (slotsBitmapLibraryAddress !== ZERO_ADDRESS) {
                        const lib = await SlotsBitmapLibrary.at(
                            slotsBitmapLibraryAddress
                        );
                        GeneralDistributionAgreementV1.link(lib);
                    }
                } catch (e) {
                    console.warn("!!! Cannot link slotsBitmapLibrary", e.toString());
                    if (protocolReleaseVersion !== "test") {
                        throw e;
                    }
                }
            } else {
                GeneralDistributionAgreementV1.link(
                    "SlotsBitmapLibrary",
                    slotsBitmapLibraryAddress
                );
            }
            gdaIsLinked = true;
        }

        const agreement = await web3tx(
            GeneralDistributionAgreementV1.new,
            "GeneralDistributionAgreementV1.new"
        )(superfluid.address, superfluidPoolBeaconAddr);

        console.log(
            "New GeneralDistributionAgreementV1 address",
            agreement.address
        );
        output += `GDA_LOGIC=${agreement.address}\n`;
        return agreement;
    };

    // initial GDA deployment (GDA bootstrapping)
    if (!(await superfluid.isAgreementTypeListed.call(GDAv1_TYPE))) {
        // first we deploy a SuperfluidPoolBeacon
        // ... with the placeholder logic (just enough to allow later update)
        const superfluidPoolPlaceholderLogic = await web3tx(
            SuperfluidPoolPlaceholder.new,
            "SuperfluidPoolPlaceholder.new"
        )();
        const superfluidPoolBeaconContract = await web3tx(
            SuperfluidUpgradeableBeacon.new,
            "SuperfluidUpgradeableBeacon.new"
        )(superfluidPoolPlaceholderLogic.address);
        console.log(
            "New SuperfluidPoolBeacon address",
            superfluidPoolBeaconContract.address
        );
        output += `SUPERFLUID_POOL_BEACON=${superfluidPoolBeaconContract.address}\n`;

        const gda = await deployGDAv1(superfluidPoolBeaconContract.address);

        /*
        // now that we have a GDA, we can deploy the actual SuperfluidPool...
        // "narrator: no, we cannot, needs the proxy address"
        const superfluidPoolLogic = await web3tx(
            SuperfluidPool.new,
            "SuperfluidPool.new"
        )(gda.address);
        await superfluidPoolLogic.castrate();
        console.log("New SuperfluidPoolLogic address", superfluidPoolLogic.address);
        output += `SUPERFLUID_POOL_LOGIC=${superfluidPoolLogic.address}\n`;

        // ...update the beacon to it...
        console.log("Upgrading beacon to the actual SuperfluidPool logic...");
        await superfluidPoolBeaconContract.upgradeTo(superfluidPoolLogic.address);
        */

        // ...and transfer ownership of the beacon
        console.log("Transferring ownership of beacon contract to Superfluid Host...");
        await superfluidPoolBeaconContract.transferOwnership(superfluid.address);

        // finally, register the GDA with a gov action.
        // its pending state changes don't affect the remaining actions
        await sendGovernanceAction(
            sfObjForGovAndResolver,
            (gov) => gov.registerAgreementClass(superfluid.address, gda.address)
        );

        // assumption: testnets don't require async gov action execution, so can continue
        // while for mainnets with async gov action, we need to exit here.
        if (!config.isTestnet) {
            console.log("info for verification:");
            console.log(output);
            console.log("##### STEP1 of GDA DEPLOYMENT DONE #####");
            console.log("Now go execute the gov action, then run this script again");
            process.exit();
        }
    } else {
        // NOTE that we are reusing the existing deployed external library
        // here as an optimization, this assumes that we do not change the
        // library code.
        // link library in order to avoid spurious code change detections
        try {
            const GDAv1 = await GeneralDistributionAgreementV1.at(
                await superfluid.getAgreementClass.call(GDAv1_TYPE)
            );
            console.log("GDAv1 proxy address", GDAv1.address);
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
                    `Governance set CFAv1Forwarder`
                )(superfluid.address, ZERO_ADDRESS, forwarder.address);
                return forwarder;
            }
        );

        // deploy IDAv1Forwarder for test deployments
        // for other (permanent) deployments, it's not handled by this script
        await deployAndRegisterContractIf(
            IDAv1Forwarder,
            "IDAv1Forwarder",
            async (contractAddress) => contractAddress === ZERO_ADDRESS,
            async () => {
                const forwarder = await IDAv1Forwarder.new(superfluid.address);
                output += `IDA_V1_FORWARDER=${forwarder.address}\n`;
                await web3tx(
                    governance.enableTrustedForwarder,
                    `Governance set IDAv1Forwarder`
                )(superfluid.address, ZERO_ADDRESS, forwarder.address);
                return forwarder;
            }
        );

        // deploy GDAv1Forwarder for test deployments
        // for other (permanent) deployments, it's not handled by this script
        await deployAndRegisterContractIf(
            GDAv1Forwarder,
            "GDAv1Forwarder",
            async (contractAddress) => contractAddress === ZERO_ADDRESS,
            async () => {
                const forwarder = await GDAv1Forwarder.new(superfluid.address);
                output += `GDA_V1_FORWARDER=${forwarder.address}\n`;
                await web3tx(
                    governance.enableTrustedForwarder,
                    `Governance set GDAv1Forwarder`
                )(superfluid.address, ZERO_ADDRESS, forwarder.address);
                return forwarder;
            }
        );
    }

    // =========== UPGRADE changed contracts ===========
    console.log("===== STARTING UPGRADE PHASE ======");

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
            [ superfluidConstructorParam ]
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
            [ superfluidConstructorParam ]
        );
        if (idaNewLogicAddress !== ZERO_ADDRESS) {
            agreementsToUpdate.push(idaNewLogicAddress);
        }
        // deploy new GDA logic
        const gdaProxyAddr = await superfluid.getAgreementClass.call(GDAv1_TYPE);
        const gdaLogicAddr = await (await UUPSProxiable.at(gdaProxyAddr)).getCodeAddress();
        const superfluidPoolBeaconAddr = await (
            await GeneralDistributionAgreementV1.at(gdaProxyAddr)
        ).superfluidPoolBeacon.call();
        const gdaNewLogicAddress = await deployContractIfCodeChanged(
            web3,
            GeneralDistributionAgreementV1,
            gdaLogicAddr,
            async () => (await deployGDAv1(superfluidPoolBeaconAddr)).address,
            [
                superfluidConstructorParam,
                ap(superfluidPoolBeaconAddr)
            ]
        );
        if (gdaNewLogicAddress !== ZERO_ADDRESS) {
            agreementsToUpdate.push(gdaNewLogicAddress);
        }
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

            if (factoryAddress === ZERO_ADDRESS) return true;

            const factory = await SuperTokenFactoryLogic.at(factoryAddress);
            const superTokenLogicAddress = await factory.getSuperTokenLogic.call();
            const superTokenLogic = await SuperTokenLogic.at(superTokenLogicAddress);

            const cfaPAddr = await superfluid.getAgreementClass.call(CFAv1_TYPE);
            const gdaPAddr = await superfluid.getAgreementClass.call(GDAv1_TYPE);

            const cofNFTPAddr = await superTokenLogic.CONSTANT_OUTFLOW_NFT();
            const cifNFTPAddr = await superTokenLogic.CONSTANT_INFLOW_NFT();

            let cofNFTLAddr;
            let cifNFTLAddr;

            if (cofNFTPAddr !== ZERO_ADDRESS) {
                const cofNFTContract = await ConstantOutflowNFT.at(cofNFTPAddr);
                cofNFTLAddr = await cofNFTContract.getCodeAddress();
                constantOutflowNFTLogicChanged = await codeChanged(
                    web3,
                    ConstantOutflowNFT,
                    cofNFTLAddr,
                    [superfluidConstructorParam, ap(cifNFTPAddr), ap(cfaPAddr), ap(gdaPAddr)]
                );
                console.log("   constantOutflowNFTLogicChanged:", constantOutflowNFTLogicChanged);
            }

            if (cifNFTPAddr !== ZERO_ADDRESS) {
                const cifNFTContract = await ConstantInflowNFT.at(cifNFTPAddr);
                cifNFTLAddr = await cifNFTContract.getCodeAddress();
                constantInflowNFTLogicChanged = await codeChanged(
                    web3,
                    ConstantInflowNFT,
                    cifNFTLAddr,
                    [superfluidConstructorParam, ap(cofNFTPAddr), ap(cfaPAddr), ap(gdaPAddr)]
                );
                console.log("   constantInflowNFTLogicChanged:", constantInflowNFTLogicChanged);
            }

            // TODO: remove from try block once all networks have a PoolNFT aware supertoken logic deployed
            try {
                const poolAdminNFTPAddr = await superTokenLogic.POOL_ADMIN_NFT();
                const poolMemberNFTPAddr = await superTokenLogic.POOL_MEMBER_NFT();
                const poolAdminNFTContract = await PoolAdminNFT.at(poolAdminNFTPAddr);
                const poolMemberNFTContract = await PoolMemberNFT.at(poolMemberNFTPAddr);
                const poolAdminNFTLAddr = await poolAdminNFTContract.getCodeAddress();
                const poolMemberNFTLAddr = await poolMemberNFTContract.getCodeAddress();


                // TODO: check only if non-zero address
                // don't do in try block, otherwise we may accidentally re-deploy the NFT proxies


                poolAdminNFTLogicChanged = await codeChanged(
                    web3,
                    PoolAdminNFT,
                    poolAdminNFTLAddr,
                    [superfluidConstructorParam, ap(gdaPAddr)]
                );
                console.log("   poolAdminNFTLogicChanged:", poolAdminNFTLogicChanged);

                poolMemberNFTLogicChanged = await codeChanged(
                    web3,
                    PoolMemberNFT,
                    poolMemberNFTLAddr,
                    [superfluidConstructorParam, ap(gdaPAddr)]
                );
                console.log("   poolMemberNFTLogicChanged:", poolMemberNFTLogicChanged);

                const superTokenFactoryCodeChanged = await codeChanged(
                    web3,
                    SuperTokenFactoryLogic,
                    await superfluid.getSuperTokenFactoryLogic.call(),
                    [superfluidConstructorParam, ap(superTokenLogicAddress), ap(cofNFTLAddr), ap(cifNFTLAddr),
                    ap(poolAdminNFTLAddr), ap(poolMemberNFTLAddr)]
                );
                console.log("   superTokenFactoryCodeChanged:", superTokenFactoryCodeChanged);

                const superTokenLogicCodeChanged = await codeChanged(
                    web3,
                    SuperTokenLogic,
                    await factory.getSuperTokenLogic.call(),
                    // this replacement does not support SuperTokenMock
                    [
                        superfluidConstructorParam, ap(cofNFTPAddr), ap(cifNFTPAddr),
                        ap(poolAdminNFTPAddr), ap(poolMemberNFTPAddr)
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
        // now we know if something changed which requires it to be upgraded
        // if so, this is what needs to be done:
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
                console.log("   getting FlowNFT addrs");
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

                // TODO: remove from try block once all networks have a PoolNFT aware supertoken logic deployed
                try {
                    // Pool NFTs
                    console.log("   getting PoolNFT addrs");
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
                    console.error("Unable to get PoolNFT proxy addresses");
                    // if any of them fails, we assume the following ones are missing too
                }
            }

            // if the super token logic does not have the proxies, we must deploy
            // new nft logic and proxies.

            const cfaAddr = await superfluid.getAgreementClass.call(CFAv1_TYPE);
            const gdaAddr = await superfluid.getAgreementClass.call(GDAv1_TYPE);

            // TODO: we may not want it deployed if address is zero (eth-mainnet)

            if (
                cofNFTProxyAddress === ZERO_ADDRESS ||
                cifNFTProxyAddress === ZERO_ADDRESS
            ) {
                console.log("BOOTSTRAPPING: Deploying Flow NFT Proxies...");
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
                    "CONSTANT_OUTFLOW_NFT_LOGIC",
                    [superfluid.address, cfaAddr, gdaAddr, constantInflowNFTProxy.address]
                );
                const constantInflowNFTLogic = await deployNFTContract(
                    ConstantInflowNFT,
                    "ConstantInflowNFT",
                    "CONSTANT_INFLOW_NFT_LOGIC",
                    [superfluid.address, cfaAddr, gdaAddr, constantOutflowNFTProxy.address]
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
            } else {
                // FlowNFT proxies already exist
                console.log("Check-upgrading Flow NFTs...");
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
                            "CONSTANT_OUTFLOW_NFT_LOGIC",
                            [superfluid.address, cfaAddr, gdaAddr, cifNFTProxyAddress]
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
                            "CONSTANT_INFLOW_NFT_LOGIC",
                            [superfluid.address, cfaAddr, gdaAddr, cofNFTProxyAddress]
                        );
                        // @note we set the cifNFTLogicAddress to be passed to SuperTokenFactoryLogic here
                        cifNFTLogicAddress = cifNFTLogic.address;
                        return cifNFTLogic.address;
                    }
                );
            };


            if (
                poolAdminNFTProxyAddress === ZERO_ADDRESS ||
                poolMemberNFTProxyAddress === ZERO_ADDRESS
            ) {
                console.log("BOOTSTRAPPING: Deploying Pool NFT Proxies...");
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
                    "POOL_ADMIN_NFT_LOGIC",
                    [superfluid.address, gdaAddr]
                );
                const poolMemberNFTLogic = await deployNFTContract(
                    PoolMemberNFT,
                    "PoolMemberNFT",
                    "POOL_MEMBER_NFT_LOGIC",
                    [superfluid.address, gdaAddr]
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
            } else {
                // PoolNFT proxies already exist
                console.log("Check-upgrading Pool NFTs...");
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
                            "POOL_ADMIN_NFT_LOGIC",
                            [superfluid.address, gdaAddr]
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
                            "POOL_MEMBER_NFT_LOGIC",
                            [superfluid.address, gdaAddr]
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
            output += `SUPER_TOKEN_LOGIC=${superTokenLogic.address}\n`;

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
            output += `SUPER_TOKEN_FACTORY_LOGIC=${superTokenFactoryLogic.address}\n`;
            return superTokenFactoryLogic.address;
        }
    );


    let superfluidPoolNewLogicAddress = ZERO_ADDRESS;

    // SuperfluidPool upgrade
    const gdaV1Contract = await GeneralDistributionAgreementV1.at(
        await superfluid.getAgreementClass.call(GDAv1_TYPE)
    );
    const superfluidPoolBeaconAddress = await gdaV1Contract.superfluidPoolBeacon();

    superfluidPoolNewLogicAddress = await deployContractIfCodeChanged(
        web3,
        SuperfluidPool,
        await (
            await SuperfluidUpgradeableBeacon.at(superfluidPoolBeaconAddress)
        ).implementation(),
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
        [ap(gdaV1Contract.address)]
    );

    if (
        superfluidNewLogicAddress !== ZERO_ADDRESS ||
        agreementsToUpdate.length > 0 ||
        superTokenFactoryNewLogicAddress !== ZERO_ADDRESS ||
        superfluidPoolNewLogicAddress !== ZERO_ADDRESS
    ) {
        console.log(`Creting gov action: gov.updateContracts(${superfluid.address}, ${superfluidNewLogicAddress},
            [${agreementsToUpdate}], ${superTokenFactoryNewLogicAddress}, ${superfluidPoolNewLogicAddress})`);

        await sendGovernanceAction(
            sfObjForGovAndResolver,
            (gov) => gov.updateContracts(
                superfluid.address,
                superfluidNewLogicAddress,
                agreementsToUpdate,
                superTokenFactoryNewLogicAddress,
                superfluidPoolNewLogicAddress
            )
        );
    }


    // finally, set the version string in resolver
    // Note that if executed immediately, this may advance the version string
    // before the actual protocol upgrade takes place through gov multisig signing
    if (previousVersionString !== versionString) {
        const encodedVersionString = versionStringToPseudoAddress(versionString);
        await setResolver(sfObjForGovAndResolver, `versionString.${protocolReleaseVersion}`, encodedVersionString);
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
