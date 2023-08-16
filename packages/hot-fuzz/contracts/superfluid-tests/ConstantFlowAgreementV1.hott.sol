// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";


abstract contract CFAHotFuzzMixin is HotFuzzBase {
    function createFlow(uint8 a, uint8 b, int64 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.flow(address(testerB), int96(flowRate));
    }

    function deleteFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.flow(address(testerB), 0);
    }
}

contract CFAHotFuzz is CFAHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        _initTesters();
    }
}
