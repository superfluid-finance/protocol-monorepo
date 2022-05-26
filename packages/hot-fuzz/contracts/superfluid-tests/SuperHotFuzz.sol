// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "./ConstantFlowAgreementV1.hott.sol";
import "./InstantDistributionAgreementV1.hott.sol";
import "./SuperToken.hott.sol";


// Combine all the hot fuzzes
contract SuperHotFuzz is CFAHotFuzzMixin, IDAHotFuzzMixin, SuperTokenHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        initTesters();
    }
}
