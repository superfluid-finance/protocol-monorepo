// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperfluid, ISuperfluidToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import { SuperTokenFactoryBase } from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperTokenFactory.sol";
import "../FoundrySuperfluidTester.sol";


contract ConstantFlowAgreementV1Anvil is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    struct InitializeData {
        address underlyingToken;
        address superToken;
    }
    constructor () FoundrySuperfluidTester(3) { }

    function testAlice2Bob(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.startPrank(alice);
        sf.cfaLib.createFlow(bob, superToken, flowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate);

        assertTrue(checkAllInvariants());
    }

    function testBobAliceLoop(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.startPrank(alice);
        sf.cfaLib.createFlow(bob, superToken, flowRate);
        vm.stopPrank();

        vm.startPrank(bob);
        sf.cfaLib.createFlow(alice, superToken, flowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), 0);
        assertEq(sf.cfa.getNetFlow(superToken, bob), 0);

        assertTrue(checkAllInvariants());
    }

    function testTokenCreateFlowFailIfNotCanonical(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.startPrank(alice);
        vm.expectRevert(ISuperfluid.HOST_UNTRUSTED_SUPER_TOKEN.selector);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();
    }

    function testTokenCreateFlow(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.startPrank(address(sfDeployer));
        SuperTokenFactoryBase.InitializeData[] memory tokens = new SuperTokenFactoryBase.InitializeData[](1);
        SuperTokenFactoryBase.InitializeData memory tokenData = SuperTokenFactoryBase.InitializeData({
            underlyingToken: superToken.getUnderlyingToken(),
            superToken: address(superToken)
        });
        tokens[0] = tokenData;
        sf.superTokenFactory.initializeCanonicalWrapperSuperTokens(tokens);
        vm.stopPrank();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate);

        assertTrue(checkAllInvariants());
    }
}
