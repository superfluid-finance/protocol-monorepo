// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import "../FoundrySuperfluidTester.sol";


contract ConstantFlowAgreementV1Anvil is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

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
}
