// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";


contract CFAHotFuzz is HotFuzzBase {
    function createFlow(uint8 a, uint8 b, uint32 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);

        testerA.flow(address(testerB), int96(uint96(flowRate)));
    }

    function deleteFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);

        testerA.flow(address(testerB), 0);
    }
}
