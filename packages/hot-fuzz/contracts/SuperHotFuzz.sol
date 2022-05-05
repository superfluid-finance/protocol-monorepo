// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import "./agreements/ConstantFlowAgreementV1.hott.sol";
import "./agreements/InstantDistributionAgreementV1.hott.sol";
import "./superfluid/SuperToken.hott.sol";


// Combine all the hot fuzzes
// solhint-disable-next-line no-empty-blocks
contract SuperHotFuzz is CFAHotFuzz, IDAHotFuzz, SuperTokenHotFuzz {}
