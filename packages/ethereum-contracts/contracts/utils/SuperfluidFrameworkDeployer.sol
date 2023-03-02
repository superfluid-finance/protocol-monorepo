// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
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
    SuperTokenFactory,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { ISuperToken, SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import { SETHProxy } from "../tokens/SETH.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";

import {
    IConstantFlowAgreementHook
} from "../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";

/// @title Superfluid Framework Deployer
/// @notice This is NOT for deploying public nets, but rather only for tesing envs
contract SuperfluidFrameworkDeployer {
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

    address public constant DEFAULT_REWARD_ADDRESS = address(69);

    TestGovernance internal testGovernance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfaV1;
    InstantDistributionAgreementV1 internal idaV1;
    SuperTokenFactory internal superTokenFactory;
    TestResolver internal testResolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;

    constructor() {
        // @note ERC1820 must be deployed for this to work

        // Deploy TestGovernance. Needs initialization later.
        testGovernance = SuperfluidGovDeployerLibrary.deployTestGovernance();

        // Transfer ownership to this contract
        SuperfluidGovDeployerLibrary.transferOwnership(
            testGovernance,
            address(this)
        );

        // Deploy Superfluid
        host = SuperfluidHostDeployerLibrary.deploySuperfluidHost(true, false);

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

        // Deploy ConstantFlowAgreementV1
        // @note TODO hook contract is no more-should be deleted

        cfaV1 = SuperfluidCFAv1DeployerLibrary.deployConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0))
        );

        // Register ConstantFlowAgreementV1 TestGovernance
        testGovernance.registerAgreementClass(host, address(cfaV1));

        // Deploy CFAv1Forwarder
        cfaV1Forwarder = new CFAv1Forwarder(host);

        // Enable CFAv1Forwarder as a Trusted Forwarder
        testGovernance.enableTrustedForwarder(
            host,
            ISuperfluidToken(address(0)),
            address(cfaV1Forwarder)
        );

        // Deploy InstantDistributionAgreementV1
        idaV1 = SuperfluidIDAv1DeployerLibrary
            .deployInstantDistributionAgreementV1(host);

        // Register InstantDistributionAgreementV1 with Governance
        testGovernance.registerAgreementClass(host, address(idaV1));

        // Deploy canonical Constant Outflow NFT logic contract
        ConstantOutflowNFT constantOutflowNFTLogic = new ConstantOutflowNFT(cfaV1);

        // Deploy canonical Constant Inflow NFT logic contract
        ConstantInflowNFT constantInflowNFTLogic = new ConstantInflowNFT(cfaV1);

        // Deploy canonical SuperToken logic contract
        SuperToken superTokenLogic = SuperToken(
            SuperTokenDeployerLibrary.deploySuperTokenLogic(
                host,
                IConstantOutflowNFT(address(constantOutflowNFTLogic)),
                IConstantInflowNFT(address(constantInflowNFTLogic))
            )
        );

        // Deploy SuperTokenFactory
        superTokenFactory = SuperfluidPeripheryDeployerLibrary
            .deploySuperTokenFactory(host, superTokenLogic);

        // 'Update' code with Governance and register SuperTokenFactory with Superfluid
        testGovernance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(superTokenFactory)
        );

        // Deploy Resolver and grant the deployer of SuperfluidFrameworkDeployer admin privileges
        testResolver = SuperfluidPeripheryDeployerLibrary.deployTestResolver(
            msg.sender
        );

        // Deploy SuperfluidLoader
        superfluidLoader = new SuperfluidLoader(testResolver);

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

    /// @notice Transfer ownership of the TestGovernance contract
    /// @dev This function allows you to transfer ownership of TestGovernance when testing
    /// @param newOwner the new owner of the TestGovernance contract
    function transferOwnership(address newOwner) public {
        testGovernance.transferOwnership(newOwner);
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
        return address(new SuperToken(host, constantOutflowNFT, constantInflowNFT));
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
        ISuperToken _superTokenLogic
    ) external returns (SuperTokenFactory) {
        return new SuperTokenFactory(_host, _superTokenLogic);
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
