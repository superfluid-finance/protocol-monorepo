// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
// solhint-disable func-name-mixedcase
pragma solidity >= 0.8.0;

import "./BaseFuzzer.sol";

contract CFAProperties is AbstractBaseFuzzer, BaseFuzzer {

    function test_maxflowrate_deposit(int96 flowRate) public {
        require(flowRate > 0);
        //assert(false);
    }

}
