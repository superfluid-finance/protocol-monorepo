// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import {
    ISuperfluid,
    ISuperfluidToken,
    Superfluid
} from "../superfluid/Superfluid.sol";
import { TestGovernance } from "./TestGovernance.sol";
import {
    ConstantFlowAgreementV1
} from "../agreements/ConstantFlowAgreementV1.sol";
import {
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT,
    IConstantInflowNFT
} from "../superfluid/ConstantInflowNFT.sol";
import {
    InstantDistributionAgreementV1
} from "../agreements/InstantDistributionAgreementV1.sol";
import {
    SuperToken,
    SuperTokenFactory
} from "../superfluid/SuperTokenFactory.sol";
import { ISuperToken, SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import {
    IConstantFlowAgreementHook
} from "../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";
import { IResolver } from "../interfaces/utils/IResolver.sol";

/// @title Superfluid Framework Deployment Steps
/// @author Superfluid
/// @notice A contract which splits framework deployment into steps.
contract SuperfluidFrameworkDeploymentSteps {
    address public constant DEFAULT_REWARD_ADDRESS = address(69);

    struct Framework {
        TestGovernance governance;
        Superfluid host;
        ConstantFlowAgreementV1 cfa;
        CFAv1Library.InitData cfaLib;
        InstantDistributionAgreementV1 ida;
        IDAv1Library.InitData idaLib;
        SuperTokenFactory superTokenFactory;
        TestResolver resolver;
        SuperfluidLoader superfluidLoader;
        CFAv1Forwarder cfaV1Forwarder;
    }

    TestGovernance internal testGovernance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfaV1;
    InstantDistributionAgreementV1 internal idaV1;
    SuperTokenFactory internal superTokenFactory;
    TestResolver internal testResolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;
    ConstantOutflowNFT internal constantOutflowNFTLogic;
    ConstantInflowNFT internal constantInflowNFTLogic;
    ConstantOutflowNFT internal constantOutflowNFT;
    ConstantInflowNFT internal constantInflowNFT;
    uint8 private currentStep;

    function _deployGovernance(address newOwner) internal {
        // Deploy TestGovernance. Needs initialization later.
        testGovernance = SuperfluidGovDeployerLibrary.deployTestGovernance();

        SuperfluidGovDeployerLibrary.transferOwnership(
            testGovernance,
            newOwner
        );
    }

    function _deployHostAndInitializeHostAndGovernance(
        bool nonUpgradable,
        bool appWhiteListingEnabled
    ) internal {
        // Deploy Host
        host = SuperfluidHostDeployerLibrary.deploySuperfluidHost(
            nonUpgradable,
            appWhiteListingEnabled
        );

        // Initialize Superfluid with Governance address
        host.initialize(testGovernance);

        // Initialize Governance
        address[] memory trustedForwarders = new address[](0);
        testGovernance.initialize(
            host,
            DEFAULT_REWARD_ADDRESS,
            4 hours,
            30 minutes,
            trustedForwarders
        );
    }

    function _deployAgreementsAndRegister() internal {
        // Deploy CFA
        cfaV1 = SuperfluidCFAv1DeployerLibrary.deployConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0))
        );

        // Deploy IDA
        idaV1 = SuperfluidIDAv1DeployerLibrary
            .deployInstantDistributionAgreementV1(host);

        // Register CFA
        testGovernance.registerAgreementClass(host, address(cfaV1));

        // Register IDA
        testGovernance.registerAgreementClass(host, address(idaV1));
    }

    function _deployCFAv1ForwarderAndEnable() internal {
        // Deploy CFAv1Forwarder
        cfaV1Forwarder = CFAv1ForwarderDeployerLibrary.deployCFAv1Forwarder(
            host
        );

        // Enable CFAv1Forwarder
        testGovernance.enableTrustedForwarder(
            host,
            ISuperfluidToken(address(0)),
            address(cfaV1Forwarder)
        );
    }


    function _deployNFTProxyAndLogicAndInitialize() internal {
        // Deploy canonical Constant Outflow NFT proxy contract
        UUPSProxy constantOutflowNFTProxy = ProxyDeployerLibrary
            .deployUUPSProxy();

        // Deploy canonical Constant Outflow NFT proxy contract
        UUPSProxy constantInflowNFTProxy = ProxyDeployerLibrary
            .deployUUPSProxy();

        // Deploy canonical Constant Outflow NFT logic contract
        constantOutflowNFTLogic = SuperfluidNFTLogicDeployerLibrary
            .deployConstantOutflowNFT(
                host,
                IConstantInflowNFT(address(constantInflowNFTProxy))
            );

        // Initialize Constant Outflow NFT logic contract
        constantOutflowNFTLogic.castrate();

        // Deploy canonical Constant Inflow NFT logic contract
        constantInflowNFTLogic = SuperfluidNFTLogicDeployerLibrary
            .deployConstantInflowNFT(
                host,
                IConstantOutflowNFT(address(constantOutflowNFTProxy))
            );

        // Initialize Constant Inflow NFT logic contract
        constantInflowNFTLogic.castrate();

        // Initialize COFNFT proxy contract
        constantOutflowNFTProxy.initializeProxy(
            address(constantOutflowNFTLogic)
        );

        // Initialize CIFNFT proxy contract
        constantInflowNFTProxy.initializeProxy(address(constantInflowNFTLogic));

        // // Initialize COFNFT proxy contract
        IConstantOutflowNFT(address(constantOutflowNFTProxy)).initialize(
            "Constant Outflow NFT",
            "COF"
        );

        // // Initialize CIFNFT proxy contract
        IConstantInflowNFT(address(constantInflowNFTProxy)).initialize(
            "Constant Inflow NFT",
            "CIF"
        );

        constantOutflowNFT = ConstantOutflowNFT(
            address(constantOutflowNFTProxy)
        );
        constantInflowNFT = ConstantInflowNFT(address(constantInflowNFTProxy));
    }

    function _deploySuperTokenLogicAndSuperTokenFactory() internal {
        // Deploy canonical SuperToken logic contract
        SuperToken superTokenLogic = SuperToken(
            SuperTokenDeployerLibrary.deploySuperTokenLogic(
                host,
                IConstantOutflowNFT(address(constantOutflowNFT)),
                IConstantInflowNFT(address(constantInflowNFT))
            )
        );

        // Deploy SuperTokenFactory
        SuperTokenFactory superTokenFactoryLogic = SuperfluidPeripheryDeployerLibrary
                .deploySuperTokenFactory(
                    host,
                    superTokenLogic,
                    constantOutflowNFTLogic,
                    constantInflowNFTLogic
                );

        // Deploy canonical Constant Outflow NFT proxy contract
        UUPSProxy superTokenFactoryProxy = ProxyDeployerLibrary
            .deployUUPSProxy();
        superTokenFactoryProxy.initializeProxy(address(superTokenFactoryLogic));

        // SuperTokenFactory(address(superTokenFactoryProxy)).initialize();

        superTokenFactory = SuperTokenFactory(address(superTokenFactoryProxy));

        // 'Update' code with Governance and register SuperTokenFactory with Superfluid
        testGovernance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(superTokenFactory)
        );
    }

    function _deployTestResolverAndSuperfluidLoaderAndSet(
        address resolverAdmin
    ) internal {
        // Deploy TestResolver
        testResolver = SuperfluidPeripheryDeployerLibrary.deployTestResolver(
            resolverAdmin
        );

        // Deploy SuperfluidLoader
        superfluidLoader = SuperfluidLoaderDeployerLibrary
            .deploySuperfluidLoader(testResolver);

        // Register Governance with Resolver
        testResolver.set("TestGovernance.test", address(testGovernance));

        // Register Superfluid with Resolver
        testResolver.set("Superfluid.test", address(host));

        // Register SuperfluidLoader with Resolver
        testResolver.set("SuperfluidLoader-v1", address(superfluidLoader));

        testResolver.set("CFAv1Forwarder", address(cfaV1Forwarder));
    }

    /// @notice Fetches the framework contracts
    function getFramework() external view returns (Framework memory sf) {
        sf = Framework({
            governance: testGovernance,
            host: host,
            cfa: cfaV1,
            cfaLib: CFAv1Library.InitData(host, cfaV1),
            ida: idaV1,
            idaLib: IDAv1Library.InitData(host, idaV1),
            superTokenFactory: superTokenFactory,
            resolver: testResolver,
            superfluidLoader: superfluidLoader,
            cfaV1Forwarder: cfaV1Forwarder
        });
        return sf;
    }

    function getNumSteps() external pure returns (uint8) {
        return _getNumSteps();
    }

    function executeStep(uint8 step) external {
        _executeStep(step);
    }

    function executeAllSteps() external {
        _executeAllSteps();
    }

    /// @notice Transfer ownership of the TestGovernance contract
    /// @dev This function allows you to transfer ownership of TestGovernance when testing
    /// @param newOwner the new owner of the TestGovernance contract
    function transferOwnership(address newOwner) public {
        testGovernance.transferOwnership(newOwner);
    }

    function _getNumSteps() internal pure returns (uint8) {
        return 8;
    }

    function _executeStep(uint8 step) internal {
        if (step != currentStep) revert("Incorrect step");

        if (step == 0) {
            // Deploy Superfluid Governance
            _deployGovernance(address(this));
        } else if (step == 1) {
            // Deploy Superfluid Host
            _deployHostAndInitializeHostAndGovernance(true, false);
        } else if (step == 2) {
            // Deploy Superfluid CFA, IDA, GDA
            _deployAgreementsAndRegister();
        } else if (step == 3) {
            // Deploy CFAv1Forwarder
            _deployCFAv1ForwarderAndEnable();
        } else if (step == 4) {
            // Deploy SuperfluidPool
            // Initialize GDA with SuperfluidPool beacon

        } else if (step == 5) {
            // Deploy Superfluid NFTs (Proxy and Logic contracts)
            _deployNFTProxyAndLogicAndInitialize();
        } else if (step == 6) {
            // Deploy SuperToken Logic
            // Deploy SuperToken Factory
            _deploySuperTokenLogicAndSuperTokenFactory();
        } else if (step == 7) {
            // Deploy TestResolver
            // Deploy SuperfluidLoader and make SuperfluidFrameworkDpeloyer an admin for the TestResolver
            _deployTestResolverAndSuperfluidLoaderAndSet(address(this));
            // Make SuperfluidFrameworkDeployer deployer an admin for the TestResolver as well
            testResolver.addAdmin(msg.sender);
        } else {
            revert("Invalid step");
        }

        currentStep++;
    }

    function _executeAllSteps() internal {
        for (uint8 i = 0; i < _getNumSteps(); ++i) {
            _executeStep(i);
        }
    }
}

/**************************************************************************
 * External Libraries
 **************************************************************************/

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
    function transferOwnership(
        TestGovernance _gov,
        address _newOwner
    ) external {
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
    function deploySuperfluidHost(
        bool _nonUpgradable,
        bool _appWhiteListingEnabled
    ) external returns (Superfluid) {
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
    function deployInstantDistributionAgreementV1(
        ISuperfluid _host
    ) external returns (InstantDistributionAgreementV1) {
        return new InstantDistributionAgreementV1(_host);
    }
}


/// @title SuperfluidCFAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid ConstantFlowAgreementV1 contract
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidCFAv1DeployerLibrary {
    /// @notice deploys ConstantFlowAgreementV1 contract
    /// @param _host address of the Superfluid contract
    /// @param _cfaHook address of the IConstantFlowAgreementHook contract
    /// @return newly deployed ConstantFlowAgreementV1 contract
    function deployConstantFlowAgreementV1(
        ISuperfluid _host,
        IConstantFlowAgreementHook _cfaHook
    ) external returns (ConstantFlowAgreementV1) {
        return new ConstantFlowAgreementV1(_host, _cfaHook);
    }
}

/// @title SuperToken deployer library
/// @author Superfluid
/// @notice This is an external library used to deploy SuperToken logic contracts
library SuperTokenDeployerLibrary {
    /// @notice Deploy a SuperToken logic contract
    /// @param host the address of the host contract
    function deploySuperTokenLogic(
        ISuperfluid host,
        IConstantOutflowNFT constantOutflowNFT,
        IConstantInflowNFT constantInflowNFT
    ) external returns (address) {
        return
            address(
                new SuperToken(host, constantOutflowNFT, constantInflowNFT)
            );
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
    /// @return newly deployed SuperTokenFactory contract
    function deploySuperTokenFactory(
        ISuperfluid _host,
        ISuperToken _superTokenLogic,
        IConstantOutflowNFT constantOutflowNFT,
        IConstantInflowNFT constantInflowNFT
    ) external returns (SuperTokenFactory) {
        return
            new SuperTokenFactory(
                _host,
                _superTokenLogic,
                constantOutflowNFT,
                constantInflowNFT
            );
    }

    /// @dev deploys Test Resolver contract
    /// @param _additionalAdmin address of the additional administrator of the Test Resolver contract
    /// @return newly deployed Test Resolver contract
    function deployTestResolver(
        address _additionalAdmin
    ) external returns (TestResolver) {
        return new TestResolver(_additionalAdmin);
    }
}

library CFAv1ForwarderDeployerLibrary {
    /// @notice deploys the Superfluid CFAv1Forwarder contract
    /// @param _host Superfluid host address
    /// @return newly deployed CFAv1Forwarder contract
    function deployCFAv1Forwarder(
        ISuperfluid _host
    ) external returns (CFAv1Forwarder) {
        return new CFAv1Forwarder(_host);
    }
}

library SuperfluidLoaderDeployerLibrary {
    /// @notice deploys the Superfluid SuperfluidLoader contract
    /// @param _resolver Superfluid resolver address
    /// @return newly deployed SuperfluidLoader contract
    function deploySuperfluidLoader(
        IResolver _resolver
    ) external returns (SuperfluidLoader) {
        return new SuperfluidLoader(_resolver);
    }
}


library SuperfluidNFTLogicDeployerLibrary {
    /// @notice deploys the Superfluid ConstantOutflowNFT contract
    /// @param _host Superfluid host address
    /// @param _constantInflowNFTProxy address of the ConstantInflowNFT proxy contract
    /// @return newly deployed ConstantOutflowNFT contract
    function deployConstantOutflowNFT(
        ISuperfluid _host,
        IConstantInflowNFT _constantInflowNFTProxy
    ) external returns (ConstantOutflowNFT) {
        return new ConstantOutflowNFT(_host, _constantInflowNFTProxy, "");
    }

    /// @notice deploys the Superfluid ConstantInflowNFT contract
    /// @param _host Superfluid host address
    /// @param _constantOutflowNFTProxy address of the ConstantOutflowNFT proxy contract
    /// @return newly deployed ConstantInflowNFT contract
    function deployConstantInflowNFT(
        ISuperfluid _host,
        IConstantOutflowNFT _constantOutflowNFTProxy
    ) external returns (ConstantInflowNFT) {
        return new ConstantInflowNFT(_host, _constantOutflowNFTProxy, "");
    }
}

library ProxyDeployerLibrary {
    function deployUUPSProxy() external returns (UUPSProxy) {
        return new UUPSProxy();
    }
}