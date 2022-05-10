// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {ERC20PresetMinterPauser} from "@openzeppelin/contracts/token/ERC20/presets/ERC20PresetMinterPauser.sol";

import {UUPSProxy} from "../upgradability/UUPSProxy.sol";

import {Superfluid} from "../superfluid/Superfluid.sol";
import {TestGovernance} from "./TestGovernance.sol";
import {ConstantFlowAgreementV1} from "../agreements/ConstantFlowAgreementV1.sol";
import {InstantDistributionAgreementV1} from "../agreements/InstantDistributionAgreementV1.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import {SuperToken} from "../superfluid/SuperToken.sol";
import "../apps/CFAv1Library.sol";
import "../apps/IDAv1Library.sol";


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
    }

    TestGovernance internal governance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfa;
    InstantDistributionAgreementV1 internal ida;
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

        // Deploy TestGovernance. Needs initialization later.
        governance = new TestGovernance();

        // Deploy Superfluid
        host = new Superfluid(true, false);

        // Initialize Superfluid with Governance address
        host.initialize(governance);

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
    }

    /// @notice Fetches the framework contracts
    function getFramework()
        external view
        returns (Framework memory sf)
    {
        sf = Framework({
            governance: governance,
            host: host,
            cfa: cfa,
            cfaLib: CFAv1Library.InitData(host, cfa),
            ida: ida,
            idaLib: IDAv1Library.InitData(host, ida),
            superTokenFactory: superTokenFactory
        });
        return sf;
    }

    /// @notice Deploy new wrapper super token
    function deployWrapperSuperToken(string calldata name, string calldata symbol)
        external
        returns (
            ERC20PresetMinterPauser token,
            SuperToken superToken
        )
    {
        token = new ERC20PresetMinterPauser(name, symbol);
        token.grantRole(token.DEFAULT_ADMIN_ROLE(), msg.sender);
        token.grantRole(token.MINTER_ROLE(), msg.sender);
        token.grantRole(token.PAUSER_ROLE(), msg.sender);

        superToken = SuperToken(address(
            superTokenFactory.createERC20Wrapper(
            ERC20WithTokenInfo(address(token)),
            ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
            string.concat(name, "x"),
            string.concat(symbol, "x"))
        ));
    }

}
