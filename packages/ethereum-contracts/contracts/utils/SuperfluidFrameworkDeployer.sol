// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {Resolver} from "../utils/Resolver.sol";
import {SuperfluidLoader} from "../utils/SuperfluidLoader.sol";
import {Superfluid} from "../superfluid/Superfluid.sol";
import {UUPSProxy} from "../upgradability/UUPSProxy.sol";
import {ConstantFlowAgreementV1} from "../agreements/ConstantFlowAgreementV1.sol";
import {InstantDistributionAgreementV1} from "../agreements/InstantDistributionAgreementV1.sol";
import {SuperTokenFactoryHelper, SuperTokenFactory} from "../superfluid/SuperTokenFactory.sol";
import {TestGovernance} from "./TestGovernance.sol";

/// @title Superfluid Framework Deployer
/// @notice This is NOT for deploying public nets, but rather only for tesing envs
contract SuperfluidFrameworkDeployer {

    /// @dev Look at all those contracts. These are used for framework deployment.
    Resolver internal resolver;
    TestGovernance internal governance;
    SuperfluidLoader internal superfluidLoader;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfa;
    InstantDistributionAgreementV1 internal ida;
    SuperTokenFactoryHelper internal superTokenFactoryHelper;
    SuperTokenFactory internal superTokenFactory;

    /// @notice Deploys everything... probably
    constructor() {
        // Make sure ERC1820 is deployed
        // TODO with foundry etched ERC1820 contract is not available yet during the same transaction while in a
        // different external call. It could be either an EVM spec behaviour, or it could be a foundry-evm behaviour.
        /* {
            uint256 cs;
            // solhint-disable-next-line no-inline-assembly
            assembly { cs := extcodesize(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24) }
            require(cs > 0, "ERC1820 not deployed");
        } */

        // Deploy Superfluid Resolver
        resolver = new Resolver();

        // Deploy TestGovernance. Needs initialization later.
        governance = new TestGovernance();

        // Register Governance with Resolver
        resolver.set("TestGovernance.test", address(governance));

        // Deploy Superfluid Loader
        superfluidLoader = new SuperfluidLoader(resolver);

        // Register SuperfluidLoader with Resolver
        resolver.set("SuperfluidLoader-v1", address(superfluidLoader));

        // Deploy Superfluid
        host = new Superfluid(true, false);

        // Initialize Superfluid with Governance address
        host.initialize(governance);

        // Register Superfluid with Resolver
        resolver.set("Superfluid.test", address(host));

        // Initialize Governance
        address[] memory trustedForarders = new address[](0);
        governance.initialize(
            host,
            address(69),
            4 hours,
            30 minutes,
            trustedForarders
        );

        // Deploy ConstantFlowAgreementV1
        cfa = new ConstantFlowAgreementV1(host);

        // Register ConstantFlowAgreementV1 TestGovernance
        governance.registerAgreementClass(host, address(cfa));

        // Deploy InstantDistributionAgreementV1
        ida = new InstantDistributionAgreementV1(host);

        // Register InstantDistributionAgreementV1 with Governance
        governance.registerAgreementClass(host, address(ida));

        // Deploy SuperTokenFactoryHelper
        superTokenFactoryHelper = new SuperTokenFactoryHelper();

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
    }

    /// @notice Fetches the framework contracts
    /// TODO returns a memory structure instead
    function getFramework()
        public
        view
        returns (
            Superfluid,
            ConstantFlowAgreementV1,
            InstantDistributionAgreementV1,
            SuperTokenFactory
        )
    {
        return (host, cfa, ida, superTokenFactory);
    }
}
