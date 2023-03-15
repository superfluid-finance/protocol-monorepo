// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { SuperTokenPool } from "../../../contracts/superfluid/SuperTokenPool.sol";

contract GeneralDistributionAgreementV1Test is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(3) { }

    function test_Create_Pool() public {
        vm.prank(alice);
        SuperTokenPool pool = new SuperTokenPool(alice);
    }
}