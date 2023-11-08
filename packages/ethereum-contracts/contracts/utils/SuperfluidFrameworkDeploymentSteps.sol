// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

// What do you expect...
// solhint-disable max-states-count

import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import { IDAv1Forwarder } from "./IDAv1Forwarder.sol";
import { GDAv1Forwarder } from "./GDAv1Forwarder.sol";
import { ISuperfluid, ISuperfluidToken, Superfluid } from "../superfluid/Superfluid.sol";
import { TestGovernance } from "./TestGovernance.sol";
import { ConstantFlowAgreementV1 } from "../agreements/ConstantFlowAgreementV1.sol";
import { ConstantOutflowNFT, IConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../agreements/gdav1/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../agreements/gdav1/PoolMemberNFT.sol";
import { InstantDistributionAgreementV1 } from "../agreements/InstantDistributionAgreementV1.sol";
import { GeneralDistributionAgreementV1 } from "../agreements/gdav1/GeneralDistributionAgreementV1.sol";
import { SuperTokenFactory } from "../superfluid/SuperTokenFactory.sol";
import { TestToken } from "./TestToken.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";
import { SETHProxy } from "../tokens/SETH.sol";
import { ISuperToken, SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import { SuperfluidPool } from "../agreements/gdav1/SuperfluidPool.sol";
import { SuperfluidUpgradeableBeacon } from "../upgradability/SuperfluidUpgradeableBeacon.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { BatchLiquidator } from "./BatchLiquidator.sol";
import { TOGA } from "./TOGA.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";
import { IResolver } from "../interfaces/utils/IResolver.sol";

/// @title Superfluid Framework Deployment Steps
/// @author Superfluid
/// @notice A contract which splits framework deployment into steps.
/// @dev This was necessary because of the contract size limit of the deployed contract
/// which is an issue when deploying the original framework with Hardhat.
/// https://github.com/NomicFoundation/hardhat/issues/3404#issuecomment-1346849400
contract SuperfluidFrameworkDeploymentSteps {
    bool public constant DEFAULT_NON_UPGRADEABLE = false;
    bool public constant DEFAULT_APP_WHITELISTING_ENABLED = false;
    address public constant DEFAULT_REWARD_ADDRESS = address(69);
    uint256 public constant DEFAULT_LIQUIDATION_PERIOD = 4 hours;
    uint256 public constant DEFAULT_PATRICIAN_PERIOD = 30 minutes;
    uint256 public constant DEFAULT_TOGA_MIN_BOND_DURATION = 1 weeks;
    // TODO we blame solidity that does not suppor this yet
    // solhint-disable-next-line var-name-mixedcase
    address[] public DEFAULT_TRUSTED_FORWARDERS = new address[](0);

    string public constant RESOLVER_BASE_SUPER_TOKEN_KEY = "supertokens.test.";
    string public constant RESOLVER_BASE_TOKEN_KEY = "tokens.test.";

    struct Framework {
        TestGovernance governance;
        Superfluid host;
        ConstantFlowAgreementV1 cfa;
        CFAv1Library.InitData cfaLib;
        InstantDistributionAgreementV1 ida;
        GeneralDistributionAgreementV1 gda;
        IDAv1Library.InitData idaLib;
        SuperTokenFactory superTokenFactory;
        ISuperToken superTokenLogic;
        ConstantOutflowNFT constantOutflowNFT;
        ConstantInflowNFT constantInflowNFT;
        TestResolver resolver;
        SuperfluidLoader superfluidLoader;
        CFAv1Forwarder cfaV1Forwarder;
        IDAv1Forwarder idaV1Forwarder;
        GDAv1Forwarder gdaV1Forwarder;
        BatchLiquidator batchLiquidator;
        TOGA toga;
    }

    uint8 private currentStep;

    // Core Contracts
    TestGovernance internal testGovernance;
    Superfluid internal host;

    // Agreement Contracts
    ConstantFlowAgreementV1 internal cfaV1;
    ConstantFlowAgreementV1 internal cfaV1Logic;
    InstantDistributionAgreementV1 internal idaV1;
    InstantDistributionAgreementV1 internal idaV1Logic;
    GeneralDistributionAgreementV1 internal gdaV1;
    GeneralDistributionAgreementV1 internal gdaV1Logic;

    // SuperToken-related Contracts
    ConstantOutflowNFT internal constantOutflowNFTLogic;
    ConstantInflowNFT internal constantInflowNFTLogic;
    ConstantOutflowNFT internal constantOutflowNFT;
    ConstantInflowNFT internal constantInflowNFT;
    PoolAdminNFT internal poolAdminNFTLogic;
    PoolMemberNFT internal poolMemberNFTLogic;
    PoolAdminNFT internal poolAdminNFT;
    PoolMemberNFT internal poolMemberNFT;
    ISuperToken internal superTokenLogic;
    SuperTokenFactory internal superTokenFactory;
    SuperTokenFactory internal superTokenFactoryLogic;

    // Peripheral Contracts
    TestResolver internal testResolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;
    IDAv1Forwarder internal idaV1Forwarder;
    GDAv1Forwarder internal gdaV1Forwarder;
    BatchLiquidator internal batchLiquidator;
    TOGA internal toga;

    error DEPLOY_AGREEMENTS_REQUIRES_DEPLOY_CORE();
    error DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_CORE();
    error DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_AGREEMENTS();
    error DEPLOY_TOGA_REQUIRES_1820();
    error DEPLOY_SUPER_TOKEN_CONTRACTS_REQUIRES_DEPLOY_CORE();
    error DEPLOY_SUPER_TOKEN_REQUIRES_1820();
    error DEPLOY_SUPER_TOKEN_REQUIRES_DEPLOY_SUPER_TOKEN_CONTRACTS();
    error RESOLVER_LIST_REQUIRES_DEPLOY_PERIPHERALS();

    /// @notice Fetches the framework contracts
    function getFramework() external view returns (Framework memory sf) {
        sf = Framework({
            governance: testGovernance,
            host: host,
            cfa: cfaV1,
            cfaLib: CFAv1Library.InitData(host, cfaV1),
            ida: idaV1,
            idaLib: IDAv1Library.InitData(host, idaV1),
            gda: gdaV1,
            superTokenFactory: superTokenFactory,
            superTokenLogic: superTokenLogic,
            constantOutflowNFT: constantOutflowNFT,
            constantInflowNFT: constantInflowNFT,
            resolver: testResolver,
            superfluidLoader: superfluidLoader,
            cfaV1Forwarder: cfaV1Forwarder,
            idaV1Forwarder: idaV1Forwarder,
            gdaV1Forwarder: gdaV1Forwarder,
            batchLiquidator: batchLiquidator,
            toga: toga
        });
        return sf;
    }

    /// @notice Transfer ownership of the TestGovernance contract
    /// @dev This function allows you to transfer ownership of TestGovernance when testing
    /// @param newOwner the new owner of the TestGovernance contract
    function transferOwnership(address newOwner) public {
        testGovernance.transferOwnership(newOwner);
    }

    function getNumSteps() public pure returns (uint8) {
        return 8;
    }

    function executeStep(uint8 step) public {
        if (step != currentStep) revert("Incorrect step");

        // CORE CONTRACTS
        if (step == 0) {
            // Deploy Superfluid Governance
            // Deploy TestGovernance. Needs initialization later.
            testGovernance = SuperfluidGovDeployerLibrary.deployTestGovernance();

            SuperfluidGovDeployerLibrary.transferOwnership(testGovernance, address(this));
        } else if (step == 1) {
            // Deploy Host
            // _deployHost(nonUpgradable, appWhiteListingEnabled);
            host = SuperfluidHostDeployerLibrary.deploySuperfluidHost(true, false);

            // _initializeHost();
            host.initialize(testGovernance);

            // _initializeGovernance(
            //     DEFAULT_REWARD_ADDRESS, DEFAULT_LIQUIDATION_PERIOD, DEFAULT_PATRICIAN_PERIOD,
            // DEFAULT_TRUSTED_FORWARDERS
            // );
            testGovernance.initialize(
                host,
                DEFAULT_REWARD_ADDRESS,
                DEFAULT_LIQUIDATION_PERIOD,
                DEFAULT_PATRICIAN_PERIOD,
                DEFAULT_TRUSTED_FORWARDERS
            );
        } else if (step == 2) {
            // AGREEMENT CONTRACTS
            // Deploy Superfluid CFA, IDA, GDA

            if (address(host) == address(0)) revert DEPLOY_AGREEMENTS_REQUIRES_DEPLOY_CORE();

            // _deployAgreementContracts();
            // _deployCFAv1();
            cfaV1Logic = SuperfluidCFAv1DeployerLibrary.deployConstantFlowAgreementV1(host);

            // _deployIDAv1();
            idaV1Logic = SuperfluidIDAv1DeployerLibrary.deployInstantDistributionAgreementV1(host);

            // _deployGDAv1();
            gdaV1Logic = SuperfluidGDAv1DeployerLibrary.deployGeneralDistributionAgreementV1(host);

            // _registerAgreements();
            // we set the canonical address based on host.getAgreementClass() because
            // in the upgradeable case, we create a new proxy contract in the function
            // and set it as the canonical agreement.
            testGovernance.registerAgreementClass(host, address(cfaV1Logic));
            cfaV1 = ConstantFlowAgreementV1(address(host.getAgreementClass(cfaV1Logic.agreementType())));
            testGovernance.registerAgreementClass(host, address(idaV1Logic));
            idaV1 = InstantDistributionAgreementV1(address(host.getAgreementClass(idaV1Logic.agreementType())));
            testGovernance.registerAgreementClass(host, address(gdaV1Logic));
            gdaV1 = GeneralDistributionAgreementV1(address(host.getAgreementClass(gdaV1Logic.agreementType())));
        } else if (step == 3) {
            // PERIPHERAL CONTRACTS: FORWARDERS
            // Deploy CFAv1Forwarder
            // _deployCFAv1Forwarder()
            cfaV1Forwarder = CFAv1ForwarderDeployerLibrary.deployCFAv1Forwarder(host);
            // _enableCFAv1ForwarderAsTrustedForwarder()
            testGovernance.enableTrustedForwarder(host, ISuperfluidToken(address(0)), address(cfaV1Forwarder));

            // Deploy IDAv1Forwarder
            // _deployIDAv1Forwarder();
            idaV1Forwarder = IDAv1ForwarderDeployerLibrary.deployIDAv1Forwarder(host);
            // _enableIDAv1ForwarderAsTrustedForwarder();
            testGovernance.enableTrustedForwarder(host, ISuperfluidToken(address(0)), address(idaV1Forwarder));

            // Deploy GDAv1Forwarder
            // _deployGDAv1Forwarder();
            gdaV1Forwarder = GDAv1ForwarderDeployerLibrary.deployGDAv1Forwarder(host);
            // _enableGDAv1ForwarderAsTrustedForwarder();
            testGovernance.enableTrustedForwarder(host, ISuperfluidToken(address(0)), address(gdaV1Forwarder));
        } else if (step == 4) {
            // PERIPHERAL CONTRACTS: SuperfluidPool Logic
            // Deploy SuperfluidPool
            // Initialize GDA with SuperfluidPool beacon
            // _deploySuperfluidPoolLogicAndInitializeGDA();

            /// Deploy SuperfluidPool logic contract
            SuperfluidPool superfluidPoolLogic = SuperfluidPoolLogicDeployerLibrary.deploySuperfluidPool(gdaV1);

            // Initialize the logic contract
            superfluidPoolLogic.castrate();

            // Deploy SuperfluidPool beacon
            SuperfluidUpgradeableBeacon superfluidPoolBeacon =
                ProxyDeployerLibrary.deploySuperfluidUpgradeableBeacon(address(superfluidPoolLogic));
            gdaV1.initialize(superfluidPoolBeacon);
            
            superfluidPoolBeacon.transferOwnership(address(host));
        } else if (step == 5) {
            // PERIPHERAL CONTRACTS: NFT Proxy and Logic
            // Deploy Superfluid NFTs (Proxy and Logic contracts)

            if (address(host) == address(0)) revert DEPLOY_SUPER_TOKEN_CONTRACTS_REQUIRES_DEPLOY_CORE();
            // Deploy canonical Constant Outflow NFT proxy contract
            UUPSProxy constantOutflowNFTProxy = ProxyDeployerLibrary.deployUUPSProxy();

            // Deploy canonical Constant Outflow NFT proxy contract
            UUPSProxy constantInflowNFTProxy = ProxyDeployerLibrary.deployUUPSProxy();

            // Deploy canonical Pool Admin NFT proxy contract
            UUPSProxy poolAdminNFTProxy = ProxyDeployerLibrary.deployUUPSProxy();

            // Deploy canonical Pool Member NFT proxy contract
            UUPSProxy poolMemberNFTProxy = ProxyDeployerLibrary.deployUUPSProxy();

            // Deploy canonical Constant Outflow NFT logic contract
            constantOutflowNFTLogic = SuperfluidFlowNFTLogicDeployerLibrary.deployConstantOutflowNFT(
                host, IConstantInflowNFT(address(constantInflowNFTProxy))
            );

            // Initialize Constant Outflow NFT logic contract
            constantOutflowNFTLogic.castrate();

            // Deploy canonical Constant Inflow NFT logic contract
            constantInflowNFTLogic = SuperfluidFlowNFTLogicDeployerLibrary.deployConstantInflowNFT(
                host, IConstantOutflowNFT(address(constantOutflowNFTProxy))
            );

            // Initialize Constant Inflow NFT logic contract
            constantInflowNFTLogic.castrate();

            // Deploy canonical Pool Admin NFT logic contract
            poolAdminNFTLogic = SuperfluidPoolNFTLogicDeployerLibrary.deployPoolAdminNFT(host);

            // Initialize Pool Admin NFT logic contract
            poolAdminNFTLogic.castrate();

            // Deploy canonical Pool Member NFT logic contract
            poolMemberNFTLogic = SuperfluidPoolNFTLogicDeployerLibrary.deployPoolMemberNFT(host);

            // Initialize Pool Member NFT logic contract
            poolMemberNFTLogic.castrate();

            // Initialize COFNFT proxy contract
            constantOutflowNFTProxy.initializeProxy(address(constantOutflowNFTLogic));

            // Initialize CIFNFT proxy contract
            constantInflowNFTProxy.initializeProxy(address(constantInflowNFTLogic));

            // Initialize Pool Admin NFT proxy contract
            poolAdminNFTProxy.initializeProxy(address(poolAdminNFTLogic));

            // Initialize Pool Member NFT proxy contract
            poolMemberNFTProxy.initializeProxy(address(poolMemberNFTLogic));

            // // Initialize COFNFT proxy contract
            IConstantOutflowNFT(address(constantOutflowNFTProxy)).initialize("Constant Outflow NFT", "COF");

            // // Initialize CIFNFT proxy contract
            IConstantInflowNFT(address(constantInflowNFTProxy)).initialize("Constant Inflow NFT", "CIF");

            // // Initialize Pool Admin NFT proxy contract
            IPoolAdminNFT(address(poolAdminNFTProxy)).initialize("Pool Admin NFT", "PA");

            // // Initialize Pool Member NFT proxy contract
            IPoolMemberNFT(address(poolMemberNFTProxy)).initialize("Pool Member NFT", "PM");

            constantOutflowNFT = ConstantOutflowNFT(address(constantOutflowNFTProxy));
            constantInflowNFT = ConstantInflowNFT(address(constantInflowNFTProxy));
            poolAdminNFT = PoolAdminNFT(address(poolAdminNFTProxy));
            poolMemberNFT = PoolMemberNFT(address(poolMemberNFTProxy));
        } else if (step == 6) {
            // PERIPHERAL CONTRACTS: SuperToken Logic and SuperTokenFactory Logic
            // Deploy SuperToken Logic
            // Deploy SuperToken Factory

            // _deploySuperTokenLogic();
            // Deploy canonical SuperToken logic contract
            superTokenLogic = SuperToken(
                SuperTokenDeployerLibrary.deploySuperTokenLogic(
                    host,
                    IConstantOutflowNFT(address(constantOutflowNFT)),
                    IConstantInflowNFT(address(constantInflowNFT)),
                    IPoolAdminNFT(address(poolAdminNFT)),
                    IPoolMemberNFT(address(poolMemberNFT))
                )
            );

            // _deploySuperTokenFactory();
            superTokenFactoryLogic = SuperfluidPeripheryDeployerLibrary.deploySuperTokenFactory(
                host,
                superTokenLogic,
                constantOutflowNFTLogic,
                constantInflowNFTLogic,
                poolAdminNFTLogic,
                poolMemberNFTLogic
            );

            // _setSuperTokenFactoryInHost();
            // 'Update' code with Governance and register SuperTokenFactory with Superfluid
            testGovernance.updateContracts(
                host, address(0), new address[](0), address(superTokenFactoryLogic), address(0)
            );

            // we set the canonical address based on host.getSuperTokenFactory() because
            // in the upgradeable case, we create a new proxy contract in the function
            // and set it as the canonical supertokenfactory.
            superTokenFactory = SuperTokenFactory(address(host.getSuperTokenFactory()));
        } else if (step == 7) {
            // PERIPHERAL CONTRACTS: Resolver, SuperfluidLoader, TOGA, BatchLiquidator
            // Deploy TestResolver
            // Deploy SuperfluidLoader and make SuperfluidFrameworkDeployer an admin for the TestResolver
            // Set TestGovernance, Superfluid, SuperfluidLoader and CFAv1Forwarder in TestResolver

            // _deployTestResolver(resolverAdmin);
            if (address(host) == address(0)) revert DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_CORE();
            testResolver = SuperfluidPeripheryDeployerLibrary.deployTestResolver(address(this));

            // _deploySuperfluidLoader();
            superfluidLoader = SuperfluidLoaderDeployerLibrary.deploySuperfluidLoader(testResolver);

            // _setAddressesInResolver();
            // Register Governance with Resolver
            testResolver.set("TestGovernance.test", address(testGovernance));

            // Register Superfluid with Resolver
            testResolver.set("Superfluid.test", address(host));

            // Register SuperfluidLoader with Resolver
            testResolver.set("SuperfluidLoader-v1", address(superfluidLoader));

            // Register CFAv1Forwarder with Resolver
            testResolver.set("CFAv1Forwarder", address(cfaV1Forwarder));

            // Register IDAv1Forwarder with Resolver
            testResolver.set("IDAv1Forwarder", address(idaV1Forwarder));

            // Register GDAv1Forwarder with Resolver
            testResolver.set("GDAv1Forwarder", address(gdaV1Forwarder));

            // Make SuperfluidFrameworkDeployer deployer an admin for the TestResolver as well
            testResolver.addAdmin(msg.sender);

            // _deployTOGA();
            if (!_is1820Deployed()) revert DEPLOY_TOGA_REQUIRES_1820();
            toga = new TOGA(host, DEFAULT_TOGA_MIN_BOND_DURATION);
            testGovernance.setRewardAddress(host, ISuperfluidToken(address(0)), address(toga));

            // _deployBatchLiquidator();
            if (address(cfaV1) == address(0)) revert DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_CORE();
            if (address(cfaV1) == address(0)) revert DEPLOY_PERIPHERALS_REQUIRES_DEPLOY_AGREEMENTS();
            batchLiquidator = new BatchLiquidator(address(host));
        } else {
            revert("Invalid step");
        }

        currentStep++;
    }

    function _is1820Deployed() internal view returns (bool) {
        uint256 codeSize;
        assembly {
            codeSize := extcodesize(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24)
        }
        return codeSize != 0;
    }
}

//// External Libraries ////

/// @title SuperfluidGovDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid TestGovernance contract with additional functions
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidGovDeployerLibrary {
    /// @notice deploys the Superfluid TestGovernance Contract
    /// @return newly deployed TestGovernance contract
    function deployTestGovernance() external returns (TestGovernance) {
        return new TestGovernance();
    }

    /// @notice transfers ownership of _gov to _newOwner
    /// @dev _gov must be deployed from this contract
    /// @param _gov address of the TestGovernance contract
    /// @param _newOwner the new owner of the governance contract
    function transferOwnership(TestGovernance _gov, address _newOwner) external {
        _gov.transferOwnership(_newOwner);
    }
}

/// @title SuperfluidHostDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid Host contract with additional functions.
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidHostDeployerLibrary {
    /// @notice Deploys the Superfluid Host Contract
    /// @param _nonUpgradable whether the hsot contract is upgradeable or not
    /// @param _appWhiteListingEnabled whether app white listing is enabled
    /// @return Superfluid newly deployed Superfluid Host contract
    function deploySuperfluidHost(bool _nonUpgradable, bool _appWhiteListingEnabled) external returns (Superfluid) {
        return new Superfluid(_nonUpgradable, _appWhiteListingEnabled);
    }
}

/// @title SuperfluidIDAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid InstantDistributionAgreementV1 contract.
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidIDAv1DeployerLibrary {
    /// @notice deploys the Superfluid InstantDistributionAgreementV1 Contract
    /// @param _host Superfluid host address
    /// @return newly deployed InstantDistributionAgreementV1 contract
    function deployInstantDistributionAgreementV1(ISuperfluid _host)
        external
        returns (InstantDistributionAgreementV1)
    {
        return new InstantDistributionAgreementV1(_host);
    }
}

/// @title SuperfluidGDAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid GeneralDistributionAgreementV1 contract
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidGDAv1DeployerLibrary {
    /// @notice deploys the Superfluid GeneralDistributionAgreementV1 Contract
    /// @param _host Superfluid host address
    /// @return newly deployed GeneralDistributionAgreementV1 contract
    function deployGeneralDistributionAgreementV1(ISuperfluid _host)
        external
        returns (GeneralDistributionAgreementV1)
    {
        return new GeneralDistributionAgreementV1(_host);
    }
}

/// @title SuperfluidCFAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid ConstantFlowAgreementV1 contract
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidCFAv1DeployerLibrary {
    /// @notice deploys ConstantFlowAgreementV1 contract
    /// @param _host address of the Superfluid contract
    /// @return newly deployed ConstantFlowAgreementV1 contract
    function deployConstantFlowAgreementV1(ISuperfluid _host) external returns (ConstantFlowAgreementV1) {
        return new ConstantFlowAgreementV1(_host);
    }
}

/// @title SuperToken deployer library
/// @author Superfluid
/// @notice This is an external library used to deploy SuperToken logic contracts
library SuperTokenDeployerLibrary {
    /// @notice Deploy a SuperToken logic contract
    /// @param host the address of the host contract
    /// @param constantOutflowNFT the address of the ConstantOutflowNFT contract
    /// @param constantInflowNFT the address of the ConstantInflowNFT contract
    /// @param poolAdminNFT the address of the PoolAdminNFT contract
    /// @param poolMemberNFT the address of the PoolMemberNFT contract
    /// @return the address of the newly deployed SuperToken logic contract
    function deploySuperTokenLogic(
        ISuperfluid host,
        IConstantOutflowNFT constantOutflowNFT,
        IConstantInflowNFT constantInflowNFT,
        IPoolAdminNFT poolAdminNFT,
        IPoolMemberNFT poolMemberNFT
    ) external returns (address) {
        return address(new SuperToken(host, constantOutflowNFT, constantInflowNFT, poolAdminNFT, poolMemberNFT));
    }
}

/// @title SuperfluidPeripheryDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid periphery contracts (Super Token Factory and Test Resolver)
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidPeripheryDeployerLibrary {
    /// @dev deploys Super Token Factory contract
    /// @param _host address of the Superfluid contract
    /// @param _superTokenLogic address of the Super Token logic contract
    /// @param constantOutflowNFTLogic address of the Constant Outflow NFT logic contract
    /// @param constantInflowNFTLogic address of the Constant Inflow NFT logic contract
    /// @param poolAdminNFTLogic address of the Pool Admin NFT logic contract
    /// @param poolMemberNFTLogic address of the Pool Member NFT logic contract
    /// @return newly deployed SuperTokenFactory contract
    function deploySuperTokenFactory(
        ISuperfluid _host,
        ISuperToken _superTokenLogic,
        IConstantOutflowNFT constantOutflowNFTLogic,
        IConstantInflowNFT constantInflowNFTLogic,
        IPoolAdminNFT poolAdminNFTLogic,
        IPoolMemberNFT poolMemberNFTLogic
    ) external returns (SuperTokenFactory) {
        return new SuperTokenFactory(
                _host,
                _superTokenLogic,
                constantOutflowNFTLogic,
                constantInflowNFTLogic,
                poolAdminNFTLogic,
                poolMemberNFTLogic
            );
    }

    /// @dev deploys Test Resolver contract
    /// @param _additionalAdmin address of the additional administrator of the Test Resolver contract
    /// @return newly deployed Test Resolver contract
    function deployTestResolver(address _additionalAdmin) external returns (TestResolver) {
        return new TestResolver(_additionalAdmin);
    }
}

library CFAv1ForwarderDeployerLibrary {
    /// @notice deploys the Superfluid CFAv1Forwarder contract
    /// @param _host Superfluid host address
    /// @return newly deployed CFAv1Forwarder contract
    function deployCFAv1Forwarder(ISuperfluid _host) external returns (CFAv1Forwarder) {
        return new CFAv1Forwarder(_host);
    }
}

library IDAv1ForwarderDeployerLibrary {
    /// @notice deploys the Superfluid IDAv1Forwarder contract
    /// @param _host Superfluid host address
    /// @return newly deployed IDAv1Forwarder contract
    function deployIDAv1Forwarder(ISuperfluid _host) external returns (IDAv1Forwarder) {
        return new IDAv1Forwarder(_host);
    }
}

library GDAv1ForwarderDeployerLibrary {
    /// @notice deploys the Superfluid GDAv1Forwarder contract
    /// @param _host Superfluid host address
    /// @return newly deployed GDAv1Forwarder contract
    function deployGDAv1Forwarder(ISuperfluid _host) external returns (GDAv1Forwarder) {
        return new GDAv1Forwarder(_host);
    }
}

library SuperfluidLoaderDeployerLibrary {
    /// @notice deploys the Superfluid SuperfluidLoader contract
    /// @param _resolver Superfluid resolver address
    /// @return newly deployed SuperfluidLoader contract
    function deploySuperfluidLoader(IResolver _resolver) external returns (SuperfluidLoader) {
        return new SuperfluidLoader(_resolver);
    }
}

library SuperfluidPoolLogicDeployerLibrary {
    /// @notice deploys the Superfluid SuperfluidPool contract
    /// @return newly deployed SuperfluidPool contract
    function deploySuperfluidPool(GeneralDistributionAgreementV1 _gda) external returns (SuperfluidPool) {
        return new SuperfluidPool(_gda);
    }
}

library SuperfluidFlowNFTLogicDeployerLibrary {
    /// @notice deploys the Superfluid ConstantOutflowNFT contract
    /// @param _host Superfluid host address
    /// @param _constantInflowNFTProxy address of the ConstantInflowNFT proxy contract
    /// @return newly deployed ConstantOutflowNFT contract
    function deployConstantOutflowNFT(ISuperfluid _host, IConstantInflowNFT _constantInflowNFTProxy)
        external
        returns (ConstantOutflowNFT)
    {
        return new ConstantOutflowNFT(_host, _constantInflowNFTProxy);
    }

    /// @notice deploys the Superfluid ConstantInflowNFT contract
    /// @param _host Superfluid host address
    /// @param _constantOutflowNFTProxy address of the ConstantOutflowNFT proxy contract
    /// @return newly deployed ConstantInflowNFT contract
    function deployConstantInflowNFT(ISuperfluid _host, IConstantOutflowNFT _constantOutflowNFTProxy)
        external
        returns (ConstantInflowNFT)
    {
        return new ConstantInflowNFT(_host, _constantOutflowNFTProxy);
    }
}

library SuperfluidPoolNFTLogicDeployerLibrary {
    /// @notice deploys the Superfluid PoolAdminNFT contract
    /// @param _host Superfluid host address
    /// @return newly deployed PoolAdminNFT contract
    function deployPoolAdminNFT(ISuperfluid _host) external returns (PoolAdminNFT) {
        return new PoolAdminNFT(_host);
    }

    /// @notice deploys the Superfluid PoolMemberNFT contract
    /// @param _host Superfluid host address
    /// @return newly deployed PoolMemberNFT contract
    function deployPoolMemberNFT(ISuperfluid _host) external returns (PoolMemberNFT) {
        return new PoolMemberNFT(_host);
    }
}

library ProxyDeployerLibrary {
    function deployUUPSProxy() external returns (UUPSProxy) {
        return new UUPSProxy();
    }

    function deploySuperfluidUpgradeableBeacon(address logicContract) external returns (SuperfluidUpgradeableBeacon) {
        return new SuperfluidUpgradeableBeacon(logicContract);
    }
}

library TokenDeployerLibrary {
    function deployTestToken(
        string calldata _underlyingName,
        string calldata _underlyingSymbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) external returns (TestToken) {
        return new TestToken(
            _underlyingName,
            _underlyingSymbol,
            _decimals,
            _mintLimit);
    }

    function deploySETHProxy() external returns (SETHProxy) {
        return new SETHProxy();
    }

    function deployPureSuperToken() external returns (PureSuperToken) {
        return new PureSuperToken();
    }
}
