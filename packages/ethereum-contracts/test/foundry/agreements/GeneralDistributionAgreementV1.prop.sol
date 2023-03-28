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
    ) public view {
        vm.assume(flowRate >= 0);
        vm.assume(settledAt > 0);
        vm.assume(settledValue > 0);
        BasicParticle memory original = BasicParticle({
            flow_rate: FlowRate.wrap(flowRate),
            settled_at: Time.wrap(settledAt),
            settled_value: Value.wrap(settledValue)
        });
        bytes32[] memory encodedData = _encodeUniversalIndex(original);
        (, BasicParticle memory decoded) = _decodeUniversalIndexData(
            encodedData
        );

        assert(
            FlowRate.unwrap(original.flow_rate) ==
                FlowRate.unwrap(decoded.flow_rate)
        );
        console.logInt(Value.unwrap(original.settled_value));
        console.logInt(Value.unwrap(decoded.settled_value));
        assert(
            Time.unwrap(original.settled_at) == Time.unwrap(decoded.settled_at)
        );
        assert(
            Value.unwrap(original.settled_value) ==
                Value.unwrap(decoded.settled_value)
        );
    }
}
