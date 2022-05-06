// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import "forge-std/Test.sol";

import {
    ISuperfluid,
    ConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";

contract ConstantFlowAgreementV1Mock is ConstantFlowAgreementV1 {
    constructor() ConstantFlowAgreementV1(ISuperfluid(address(0))) {}

    function getMaximumFlowRateFromDepositPure(
        uint256 liquidationPeriod,
        uint256 deposit)
        external pure
        returns (int96 flowRate)
    {
        return _getMaximumFlowRateFromDepositPure(liquidationPeriod, deposit);
    }

    function getDepositRequiredForFlowRatePure(
        uint256 minimumDeposit,
        uint256 liquidationPeriod,
        int96 flowRate)
        external pure
        returns (uint256 deposit)
    {
        return _getDepositRequiredForFlowRatePure(minimumDeposit, liquidationPeriod, flowRate);
    }
}

contract ConstantFlowAgreementV1Properties is Test {
    ConstantFlowAgreementV1Mock immutable private cfa;

    constructor() {
        cfa = new ConstantFlowAgreementV1Mock();
    }

    function testMaximumFlowRateAllowedForDeposit(
        uint32 liquidationPeriod,
        uint96 depositAllowed)
        public
    {
        depositAllowed = uint96(bound(uint256(depositAllowed), cfa.DEFAULT_MINIMUM_DEPOSIT(), cfa.MAXIMUM_DEPOSIT()));
        vm.assume(liquidationPeriod > 0);

        int96 flowRate = cfa.getMaximumFlowRateFromDepositPure(liquidationPeriod, depositAllowed);
        assert(flowRate > 0);
        // let's also assume these edge cases away
        vm.assume(uint256(liquidationPeriod) * uint256(uint96(flowRate)) <= cfa.MAXIMUM_FLOW_RATE());
        uint256 deposit = cfa.getDepositRequiredForFlowRatePure(0, liquidationPeriod, flowRate);
        assert(uint256(depositAllowed) >= deposit);
    }

    function testMinimumDeposit(
        uint64 minimumDeposit,
        uint32 liquidationPeriod,
        int96 flowRate)
        public
    {
        minimumDeposit = uint32(bound(uint256(minimumDeposit), cfa.DEFAULT_MINIMUM_DEPOSIT(), type(uint64).max));
        vm.assume(liquidationPeriod > 0);
        vm.assume(flowRate > 0);
        vm.assume(uint256(liquidationPeriod) * uint256(uint96(flowRate)) <= cfa.MAXIMUM_FLOW_RATE());

        uint256 deposit = cfa.getDepositRequiredForFlowRatePure(minimumDeposit, liquidationPeriod, flowRate);
        assert(deposit >= minimumDeposit);
    }
}
