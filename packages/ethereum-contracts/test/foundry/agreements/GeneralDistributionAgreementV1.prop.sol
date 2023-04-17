// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import {
    IBeacon
} from "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";
import "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";

import {
    GeneralDistributionAgreementV1,
    ISuperfluid
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";

contract GeneralDistributionAgreementV1Mock is GeneralDistributionAgreementV1 {
    constructor()
        GeneralDistributionAgreementV1(
            ISuperfluid(address(0)),
            IBeacon(address(0))
        )
    {}
}

contract GeneralDistributionAgreementV1Properties is
    GeneralDistributionAgreementV1Mock,
    Test
{
    function test_Universal_Index_Data_Encode_Decode(
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

    function test_Pool_Encode_Decode(address pool) public {
        vm.assume(pool != address(0));
        bytes32[] memory encodedData = _encodePoolData(pool);
        (, address decoded) = _decodePoolData(uint256(encodedData[0]));

        assertEq(pool, decoded);
    }

    function test_Flow_Distribution_Data_Encode_Decode(
        int96 flowRate,
        uint96 buffer
    ) public {
        vm.assume(flowRate >= 0);
        vm.assume(buffer >= 0);
        GeneralDistributionAgreementV1.FlowDistributionData
            memory original = GeneralDistributionAgreementV1
                .FlowDistributionData({ flowRate: flowRate, buffer: buffer });
        bytes32[] memory encodedData = _encodeFlowDistributionData(original);
        (
            ,
            GeneralDistributionAgreementV1.FlowDistributionData memory decoded
        ) = _decodeFlowDistributionData(uint256(encodedData[0]));

        assertEq(original.flowRate, decoded.flowRate);
        assertEq(original.buffer, decoded.buffer);
    }
}
