// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { console, Test } from "forge-std/Test.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import {
    IConstantFlowAgreementV1,
    ConstantFlowAgreementV1,
    IConstantFlowAgreementHook
} from "../../../contracts/agreements/ConstantFlowAgreementV1.sol";
import {
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT,
    IConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    IInstantDistributionAgreementV1
} from "../../../contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { IResolver } from "../../../contracts/interfaces/utils/IResolver.sol";
import {
    ISuperfluidGovernance
} from "../../../contracts/interfaces/superfluid/ISuperfluidGovernance.sol";
import {
    SuperfluidLoader
} from "../../../contracts/utils/SuperfluidLoader.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    IERC20,
    ISuperToken,
    SuperToken
} from "../../../contracts/superfluid/SuperToken.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory
} from "../../../contracts/superfluid/SuperTokenFactory.sol";
import {
    SuperTokenFactoryUpdateLogicContractsTester
} from "../../../contracts/mocks/SuperTokenFactoryMock.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";
import { ForkSmokeTest } from "./ForkSmoke.t.sol";

/// @title ForkPolygonSuperTokenFactoryUpgradeTest
/// @author Superfluid
/// @notice Tests the SuperTokenFactory upgrade flow on Polygon mainnet fork
/// @dev Note that this test file is likely dynamic and will change over time
/// due to the possibility that the upgrade flow may also change over time
/// This is also only running tests for Polygon
contract ForkPolygonSuperTokenFactoryUpgradeTest is ForkSmokeTest {
    using SuperTokenV1Library for ISuperToken;
    string public PROVIDER_URL;

    IResolver public constant resolver =
        IResolver(0xE0cc76334405EE8b39213E620587d815967af39C);

    ISuperfluid public host;
    IConstantFlowAgreementV1 public cfaV1;
    SuperfluidLoader public superfluidLoader;
    ISuperTokenFactory public superTokenFactory;
    ISuperfluidGovernance public governance;

    IERC20 public constant weth =
        IERC20(0x7ceB23fD6bC0adD59E62ac25578270cFf1b9f619);
    ISuperToken public constant ethX =
        ISuperToken(0x27e1e4E6BC79D93032abef01025811B7E4727e85);

    // arbitrary test account with a good amount of ETHx
    address public constant TEST_ACCOUNT =
        0x0154d25120Ed20A516fE43991702e7463c5A6F6e;
    address public constant ALICE = address(1);
    address public constant BOB = address(2);
    address public constant DEFAULT_FLOW_OPERATOR = address(69);

    constructor()
        ForkSmokeTest(
            ethX,
            TEST_ACCOUNT,
            resolver,
            "POLYGON_MAINNET_PROVIDER_URL"
        )
    {}

    function setUp() public {
        superfluidLoader = sfFramework.superfluidLoader;
        cfaV1 = sfFramework.cfaV1;
        host = sfFramework.host;
        governance = sfFramework.governance;
        superTokenFactory = sfFramework.superTokenFactory;

        // execute super token factory upgrade
        helper_Execute_Super_Token_Factory_Upgrade();
    }

    function helper_Execute_Super_Token_Factory_Upgrade() public {
        address superTokenFactoryLogicPre = host.getSuperTokenFactoryLogic();
        address superTokenLogicPre = address(
            superTokenFactory.getSuperTokenLogic()
        );

        address governanceOwner = Ownable(address(governance)).owner();

        // Prank as governance owner
        vm.startPrank(governanceOwner);

        ConstantOutflowNFT constantOutflowNFT = new ConstantOutflowNFT();
        ConstantInflowNFT constantInflowNFT = new ConstantInflowNFT();

        // As part of the new ops flow, we deploy a new SuperToken logic contract
        // and as part of the new NFT ops flow, we deploy new NFT logic contracts
        SuperToken newSuperTokenLogic = new SuperToken(
            host,
            IConstantOutflowNFT(address(constantOutflowNFT)),
            IConstantInflowNFT(address(constantInflowNFT))
        );

        // Deploy the new super token factory logic contract, note that we pass in
        // the new super token logic contract, this is set as an immutable field in
        // the constructor
        SuperTokenFactoryUpdateLogicContractsTester newLogic = new SuperTokenFactoryUpdateLogicContractsTester(
                host,
                newSuperTokenLogic
            );

        // update the super token factory logic via goverance->host
        governance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(newLogic)
        );

        // get the addresses of the super token factory logic and super token logic post update
        address superTokenFactoryLogicPost = host.getSuperTokenFactoryLogic();
        address superTokenLogicPost = address(
            superTokenFactory.getSuperTokenLogic()
        );

        // validate that the logic contracts have been updated and are no longer the same
        // as prior to deployment
        assertFalse(superTokenFactoryLogicPre == superTokenFactoryLogicPost);
        assertFalse(superTokenLogicPre == superTokenLogicPost);

        // validate that the super token logic is the new one
        // we deprecate the previous _superTokenLogic in slot 2 and replace it
        // with an immutable variable - this is a sanity check that the new
        // immutable variable is properly set and referenced
        assertEq(address(newSuperTokenLogic), superTokenLogicPost);

        // expect revert when trying to initialize the logic contracts
        vm.expectRevert("Initializable: contract is already initialized");
        SuperTokenFactory(superTokenFactoryLogicPost).initialize();

        vm.stopPrank();

        // the mock contract adds a new storage variable and sets it to 69
        assertEq(
            SuperTokenFactoryUpdateLogicContractsTester(
                address(superTokenFactory)
            ).newVariable(),
            0
        );

        vm.stopPrank();

        // create update and delete flows after updating SuperTokenFactory logic
        // after deploying and setting new SuperToken logic in SuperTokenFactory
        helper_Create_Update_Delete_Flow_One_To_One(ethX, TEST_ACCOUNT);

        // LOGGING
        console.log("Chain ID:                                  ", block.chainid);
        console.log("Governance Owner Address:                  ", governanceOwner);
        console.log("SuperfluidLoader Address:                  ", address(superfluidLoader));
        console.log("Superfluid Host Address:                   ", address(host));
        console.log("Superfluid Governance Address:             ", address(governance));
        console.log("SuperTokenFactory Address:                 ", address(superTokenFactory));
        console.log("SuperTokenFactoryLogic Pre Migration:      ", superTokenFactoryLogicPre);
        console.log("SuperTokenFactoryLogic Post Migration:     ", superTokenFactoryLogicPost);
        console.log("SuperTokenLogic Pre Migration:             ", superTokenLogicPre);
        console.log("SuperTokenLogic Post Migration:            ", superTokenLogicPost);
    }
}
