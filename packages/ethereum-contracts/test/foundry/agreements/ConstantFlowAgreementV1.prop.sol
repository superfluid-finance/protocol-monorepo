// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import "forge-std/Test.sol";

import {
    ISuperfluid,
    ConstantFlowAgreementV1
} from "@superfluid-finance/ethereum-contracts/contracts/agreements/ConstantFlowAgreementV1.sol";

contract ConstantFlowAgreementV1Mock is ConstantFlowAgreementV1 {
    constructor() ConstantFlowAgreementV1(ISuperfluid(address(0))) {}

    function encodeFlowData(FlowData memory flowData) external pure
        returns (uint256 wordA)
    {
        return uint256(_encodeFlowData(flowData)[0]);
    }

    function decodeFlowData(uint256 wordA) external pure
        returns (bool exist, FlowData memory flowData)
    {
        return _decodeFlowData(wordA);
    }

    function encodeFlowOperatorData(FlowOperatorData memory flowOperatorData) external pure
        returns (uint256 wordA)
    {
        return uint256(_encodeFlowOperatorData(flowOperatorData)[0]);
    }

    function decodeFlowOperatorData(uint256 wordA) external pure
        returns (bool exist, FlowOperatorData memory flowOperatorData)
    {
        return _decodeFlowOperatorData(wordA);
    }

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

    function testFlowDataEncoding(uint32 timestamp, int96 flowRate, uint64 depositClipped, uint64 owedDepositClipped)
        public view
    {
        ConstantFlowAgreementV1.FlowData memory a = ConstantFlowAgreementV1.FlowData({
            timestamp: uint256(timestamp),
                    flowRate: flowRate,
                    deposit: uint256(uint64(depositClipped << 32)),
                    owedDeposit: uint256(uint64(owedDepositClipped << 32))});
        uint256 wordA = cfa.encodeFlowData(a);
        bool exist;
        ConstantFlowAgreementV1.FlowData memory b;
        (exist, b) = cfa.decodeFlowData(wordA);
        assert(a.timestamp == b.timestamp);
        assert(a.flowRate == b.flowRate);
        assert(a.deposit == b.deposit);
        assert(a.owedDeposit == b.owedDeposit);
    }

    function testFlowOperatorDataEncoding(uint8 permissions, int96 flowRateAllowance) public {
        vm.assume(flowRateAllowance > 0);
        ConstantFlowAgreementV1.FlowOperatorData memory a = ConstantFlowAgreementV1.FlowOperatorData({
            permissions: permissions,
            flowRateAllowance: flowRateAllowance
        });
        uint256 wordA = cfa.encodeFlowOperatorData(a);
        bool exist;
        ConstantFlowAgreementV1.FlowOperatorData memory b;
        (exist, b) = cfa.decodeFlowOperatorData(wordA);
        assert(a.permissions == b.permissions);
        assert(a.flowRateAllowance == b.flowRateAllowance);
    }
}
