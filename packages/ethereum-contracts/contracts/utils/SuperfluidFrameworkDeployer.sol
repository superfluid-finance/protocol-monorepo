// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    SuperfluidGovDeployerLibrary
} from "./deployers/SuperfluidGovDeployerLibrary.sol";
import {
    SuperfluidHostDeployerLibrary
} from "./deployers/SuperfluidHostDeployerLibrary.sol";
import {
    SuperfluidCFAv1DeployerLibrary
} from "./deployers/SuperfluidCFAv1DeployerLibrary.sol";
import {
    SuperfluidIDAv1DeployerLibrary
} from "./deployers/SuperfluidIDAv1DeployerLibrary.sol";
import {
    SuperTokenDeployerLibrary
} from "../libs/SuperTokenDeployerLibrary.sol";
import {
    SuperfluidPeripheryDeployerLibrary
} from "./deployers/SuperfluidPeripheryDeployerLibrary.sol";
import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import { Superfluid } from "../superfluid/Superfluid.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { TestGovernance } from "./TestGovernance.sol";
import {
    ConstantFlowAgreementV1
} from "../agreements/ConstantFlowAgreementV1.sol";
import {
    InstantDistributionAgreementV1
} from "../agreements/InstantDistributionAgreementV1.sol";
import {
    SuperTokenFactory,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
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

        // Deploy canonical SuperToken logic contract
        SuperToken superTokenLogic = SuperToken(SuperTokenDeployerLibrary
            .deploySuperTokenLogic(host));

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
