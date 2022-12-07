// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

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
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper,
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

    TestGovernance internal governance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfa;
    InstantDistributionAgreementV1 internal ida;
    SuperTokenFactory internal superTokenFactory;
    TestResolver internal resolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;

    constructor() {
        // @note ERC1820 must be deployed for this to work

        // Deploy TestGovernance. Needs initialization later.
        governance = new TestGovernance();

        // Deploy Superfluid
        host = new Superfluid(true, false);

        // Initialize Superfluid with Governance address
        host.initialize(governance);

        // Initialize Governance
        address[] memory trustedForwarders = new address[](0);
        governance.initialize(
            host,
            address(69),
            4 hours,
            30 minutes,
            trustedForwarders
        );

        // Deploy ConstantFlowAgreementV1
        // TODO @note Once we have the actual implementation for the hook contract,
        // we will need to deploy it and put it here
        cfa = new ConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0))
        );

        // Register ConstantFlowAgreementV1 TestGovernance
        governance.registerAgreementClass(host, address(cfa));

        // Deploy CFAv1Forwarder
        cfaV1Forwarder = new CFAv1Forwarder(host);

        // Enable CFAv1Forwarder as a Trusted Forwarder
        governance.enableTrustedForwarder(
            host,
            ISuperfluidToken(address(0)),
            address(cfaV1Forwarder)
        );

        // Deploy InstantDistributionAgreementV1
        ida = new InstantDistributionAgreementV1(host);

        // Register InstantDistributionAgreementV1 with Governance
        governance.registerAgreementClass(host, address(ida));

        // Deploy SuperTokenFactoryHelper
        SuperTokenFactoryHelper superTokenFactoryHelper = new SuperTokenFactoryHelper();

        // Deploy SuperTokenFactory
        superTokenFactory = new SuperTokenFactory(
            host,
            superTokenFactoryHelper
        );

        // 'Update' code with Governance and register SuperTokenFactory with Superfluid
        governance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(superTokenFactory)
        );

        // Deploy Resolver and grant the deployer of SuperfluidFrameworkDeployer admin privileges
        resolver = new TestResolver(msg.sender);

        // Deploy SuperfluidLoader
        superfluidLoader = new SuperfluidLoader(resolver);

        // Register Governance with Resolver
        resolver.set("TestGovernance.test", address(governance));

        // Register Superfluid with Resolver
        resolver.set("Superfluid.test", address(host));

        // Register SuperfluidLoader with Resolver
        resolver.set("SuperfluidLoader-v1", address(superfluidLoader));

        resolver.set("CFAv1Forwarder", address(cfaV1Forwarder));
    }

    /// @notice Fetches the framework contracts
    function getFramework() external view returns (Framework memory sf) {
        sf = Framework({
            governance: governance,
            host: host,
            cfa: cfa,
            cfaLib: CFAv1Library.InitData(host, cfa),
            ida: ida,
            idaLib: IDAv1Library.InitData(host, ida),
            superTokenFactory: superTokenFactory,
            resolver: resolver,
            superfluidLoader: superfluidLoader,
            cfaV1Forwarder: cfaV1Forwarder
        });
        return sf;
    }
}
