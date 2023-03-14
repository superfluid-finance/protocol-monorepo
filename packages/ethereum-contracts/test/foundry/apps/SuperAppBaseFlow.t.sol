// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/console.sol";
import "../FoundrySuperfluidTester.sol";
import { SuperAppBaseFlow } from "@superfluid-finance/ethereum-contracts/contracts/apps/SuperAppBaseFlow.sol";
import { SuperAppBaseFlowTester } from "@superfluid-finance/ethereum-contracts/contracts/mocks/SuperAppBaseFlowTester.sol";
import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

contract SuperAppBaseFlowTest is FoundrySuperfluidTester {
    SuperAppBaseFlowTester superApp;
    ISuperToken otherSuperToken;

    constructor () FoundrySuperfluidTester(3) { }

    function setUp() public override virtual {
        super.setUp();
        vm.startPrank(admin);
        superApp = new SuperAppBaseFlowTester(sf.host);
        superApp.setAcceptedSuperToken(superToken, true);
        otherSuperToken = sfDeployer.deployPureSuperToken("FTT", "FTT", 1e27);
        otherSuperToken.transfer(alice, 1e21);
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
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.createFlow,
                (superToken, address(superApp), int96(69), new bytes(0))
            ),
            new bytes(0) // userData
        );
        vm.stopPrank();
    }

    function testFlowOfNotAcceptedSuperTokenToSuperApp() public {
        vm.startPrank(alice);
        vm.expectRevert();
        //vm.expectRevert(SuperAppBaseFlow.NotAcceptedSuperToken.selector);
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(
                sf.cfa.createFlow,
                (otherSuperToken, address(superApp), int96(69), new bytes(0))
            ),
            new bytes(0) // userData
        );
        vm.stopPrank();
    }
}