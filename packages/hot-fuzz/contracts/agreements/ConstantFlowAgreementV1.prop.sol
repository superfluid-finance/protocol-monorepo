// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
// solhint-disable func-name-mixedcase
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";

contract CFAProperties is HotFuzzBase {
    function testMaximumFlowRateAllowedForDeposit(
        uint32 liquidationPeriod,
        uint96 depositAllowed)
        public view
    {
        require(liquidationPeriod > 0, "FOUNDRY::ASSUME");
        require(depositAllowed >= uint96(1 << 32), "FOUNDRY::ASSUME");
        require(uint256(liquidationPeriod) * uint256(depositAllowed)
            <= uint256(uint96(type(int96).max)), "FOUNDRY::ASSUME");
        int96 flowRate = cfa.getMaximumFlowRateFromDepositPure(liquidationPeriod, depositAllowed);
        assert(flowRate > 0);
        uint256 deposit = cfa.getDepositRequiredForFlowRatePure(0, liquidationPeriod, flowRate);
        assert(uint256(depositAllowed) >= deposit);
    }

    function testMinimumDeposit(
        uint64 minimumDeposit,
        uint32 liquidationPeriod,
        int96 flowRate)
        public view
    {
        require(uint96(minimumDeposit) >= uint96(1 << 32), "FOUNDRY::ASSUME");
        require(liquidationPeriod > 0, "FOUNDRY::ASSUME");
        require(flowRate > 0, "FOUNDRY::ASSUME");
        require(uint256(liquidationPeriod) * uint256(uint96(flowRate))
            <= uint256(uint96(type(int96).max)), "FOUNDRY::ASSUME");
        uint256 deposit = cfa.getDepositRequiredForFlowRatePure(minimumDeposit, liquidationPeriod, flowRate);
        assert(deposit >= minimumDeposit);
    }
}
