// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import "forge-std/console.sol";
import "../FoundrySuperfluidTester.sol";
import { SuperAppBaseFlow } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBaseFlow.sol";
import { SuperAppBaseFlowTester } from "@superfluid-finance/ethereum-contracts/contracts/mocks/SuperAppBaseFlowTester.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import { SuperTokenV1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperTokenV1Library.sol";

contract SuperAppBaseFlowTest is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;
    using SuperTokenV1Library for ISuperToken;
    using SuperTokenV1Library for SuperToken;

    SuperAppBaseFlowTester superApp;
    ISuperToken otherSuperToken;

    constructor () FoundrySuperfluidTester(3) { }

    function setUp() public override virtual {
        super.setUp();
        vm.startPrank(admin);
        superApp = new SuperAppBaseFlowTester(sf.host);
        superApp.setAcceptedSuperToken(superToken, true);
        (, otherSuperToken) = sfDeployer.deployWrapperSuperToken("FTT", "FTT", 18, type(uint256).max);
        vm.stopPrank();
    }

    function testSetAcceptedToken() public {
        assertTrue(superApp.isAcceptedSuperToken(superToken), "primary SuperToken accepted");
        assertFalse(superApp.isAcceptedSuperToken(otherSuperToken), "other SuperToken not accepted");
        superApp.setAcceptedSuperToken(otherSuperToken, true);
        assertTrue(superApp.isAcceptedSuperToken(otherSuperToken), "other SuperToken now accepted");
    }

    function testFlowToSuperApp() public {
        vm.startPrank(alice);
        superToken.createFlow(address(superApp), int96(69));
        vm.stopPrank();
    }

    function testFlowOfNotAcceptedSuperTokenToSuperApp() public {
        vm.startPrank(alice);
        vm.expectRevert();
        //vm.expectRevert(SuperAppBaseFlow.UnauthorizedHost.selector);
        otherSuperToken.createFlow(address(superApp), int96(69));
        vm.stopPrank();
    }
}