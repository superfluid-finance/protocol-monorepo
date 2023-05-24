// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";

contract ConstantFlowAgreementV1IntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    constructor() FoundrySuperfluidTester(3) { }

    function testAlice2Bob(int96 flowRate) public {
        _helperCreateFlow(alice, bob, flowRate);

        assertEq(sf.cfa.getNetFlow(superToken, alice), -flowRate, "CFAv1.t: alice net flow != -flowRate");
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate, "CFAv1.t: bob net flow != flowRate");

        _assertGlobalInvariants();
    }

    function testBobAliceLoop(int96 flowRate) public {
        _helperCreateFlow(alice, bob, flowRate);

        _helperCreateFlow(bob, alice, flowRate);

        assertEq(sf.cfa.getNetFlow(superToken, alice), 0, "CFAv1.t: alice net flow != 0");
        assertEq(sf.cfa.getNetFlow(superToken, bob), 0, "CFAv1.t: bob net flow != 0");

        _assertGlobalInvariants();
    }
}
