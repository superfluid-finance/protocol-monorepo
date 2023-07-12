// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import { GDAv1Forwarder } from "../../../contracts/utils/GDAv1Forwarder.sol";
import {ISuperfluidPool} from "../../../contracts/interfaces/superfluid/ISuperfluidPool.sol";
import "forge-std/Test.sol";

contract GDAv1ForwarderIntegrationTest is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(3) { }

    function testGDAv1ForwarderCreateIndex() external {
        vm.startPrank(alice);
        ISuperfluidPool pool = sf.gda.createPool(superToken, alice);
        vm.stopPrank();

        vm.startPrank(bob);
        sf.gdaV1Forwarder.connectPool(pool, new bytes(0));
        vm.stopPrank();

        assertTrue(sf.gdaV1Forwarder.isMemberConnected(pool, bob));
    }
}
