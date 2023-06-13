// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";

contract ConstantFlowAgreementV1IntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    constructor() FoundrySuperfluidTester(3) { }

    function testAlice2Bob(int96 flowRate) public {
        _helperCreateFlow(superToken, alice, bob, flowRate);

        _warpAndAssertAll(superToken);
    }

    function testBobAliceLoop(int96 flowRate) public {
        _helperCreateFlow(superToken, alice, bob, flowRate);
        
        _warpAndAssertAll(superToken);

        _helperCreateFlow(superToken, bob, alice, flowRate);

        _warpAndAssertAll(superToken);
    }
}
