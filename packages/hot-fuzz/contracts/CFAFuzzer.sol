// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "./BaseFuzzer.sol";

abstract contract AbstractCFAFuzzer is AbstractBaseFuzzer {

    function createFlow(uint8 a, uint8 b, uint32 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTester2(a, b);

        testerA.flow(address(testerB), int96(uint96(flowRate)));
    }

    function deleteFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTester2(a, b);

        testerA.flow(address(testerB), 0);
    }

}

// solhint-disable-next-line no-empty-blocks
contract CFAFuzzer is AbstractCFAFuzzer, BaseFuzzer { }
