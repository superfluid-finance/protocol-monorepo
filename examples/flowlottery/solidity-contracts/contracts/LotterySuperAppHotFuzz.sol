// SPDX-License-Identifier: MIT
// solhint-disable not-rely-on-time
pragma solidity 0.8.13;

import "@superfluid-finance/hot-fuzz/contracts/HotFuzzBase.sol";

contract LotterySuperAppHotFuzz is HotFuzzBase {
    function createFlow(uint8 a, uint8 b, uint32 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);

        testerA.flow(address(testerB), int96(uint96(flowRate)));
    }
}

