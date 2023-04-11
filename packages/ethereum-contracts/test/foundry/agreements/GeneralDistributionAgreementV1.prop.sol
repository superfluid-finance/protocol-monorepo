// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";

import {
    GeneralDistributionAgreementV1,
    ISuperfluid
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";

contract GeneralDistributionAgreementV1Mock is GeneralDistributionAgreementV1 {
    constructor() GeneralDistributionAgreementV1(ISuperfluid(address(0))) {}
}

contract GeneralDistributionAgreementV1Properties is
    GeneralDistributionAgreementV1Mock,
    Test
{
    function test_Universal_Index_Data_Encoding(
        int96 flowRate,
        uint32 settledAt,
        int256 settledValue
    ) public {
        vm.assume(flowRate >= 0);
        vm.assume(settledAt > 0);
        vm.assume(settledValue > 0);
        BasicParticle memory original = BasicParticle({
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_at: Time.wrap(settledAt),
            _settled_value: Value.wrap(settledValue)
        });
        bytes32[] memory encodedData = _encodeUniversalIndex(original);
        (, BasicParticle memory decoded) = _decodeUniversalIndexData(
            encodedData
        );

        assertEq(
            FlowRate.unwrap(original._flow_rate),
            FlowRate.unwrap(decoded._flow_rate)
        );
        console.logInt(Value.unwrap(original._settled_value));
        console.logInt(Value.unwrap(decoded._settled_value));
        assertEq(
            Time.unwrap(original._settled_at),
            Time.unwrap(decoded._settled_at)
        );
        assertEq(
            Value.unwrap(original._settled_value),
            Value.unwrap(decoded._settled_value)
        );
    }
}
