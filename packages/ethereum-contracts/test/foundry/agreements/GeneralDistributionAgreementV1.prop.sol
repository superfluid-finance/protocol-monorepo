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
            ISuperfluid(address(0))
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
        int256 settledValue,
        uint96 buffer,
        bool isPool
    ) public {
        vm.assume(flowRate >= 0);
        vm.assume(settledAt > 0);
        vm.assume(settledValue > 0);
        BasicParticle memory ogParticle = BasicParticle({
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_at: Time.wrap(settledAt),
            _settled_value: Value.wrap(settledValue)
        });
        bytes32[] memory encodedData = _encodeUniversalIndexData(ogParticle, buffer, isPool);
        (, UniversalIndexData memory decoded) = _decodeUniversalIndexData(
            encodedData
        );

        assertEq(
            FlowRate.unwrap(ogParticle._flow_rate),
            decoded.flowRate,
            "flow rate mismatch"
        );
        assertEq(
            Time.unwrap(ogParticle._settled_at),
            decoded.settledAt,
            "settled at mismatch"
        );
        assertEq(
            buffer, decoded.totalBuffer,
            "total buffer mismatch"
        );
        assertEq(
            Value.unwrap(ogParticle._settled_value),
            decoded.settledValue,
            "settled value mismatch"
        );
        assertEq(
            isPool, decoded.isPool
        );
    }

    function test_Flow_Distribution_Data_Encode_Decode(
        int96 flowRate,
        uint96 buffer
    ) public {
        vm.assume(flowRate >= 0);
        vm.assume(buffer >= 0);
        GeneralDistributionAgreementV1.FlowDistributionData
            memory original = GeneralDistributionAgreementV1
                .FlowDistributionData({ flowRate: flowRate, lastUpdated: uint32(block.timestamp), buffer: buffer });
        bytes32[] memory encodedData = _encodeFlowDistributionData(original);
        (
            ,
            GeneralDistributionAgreementV1.FlowDistributionData memory decoded
        ) = _decodeFlowDistributionData(uint256(encodedData[0]));

        assertEq(original.flowRate, decoded.flowRate);
        assertEq(original.buffer, decoded.buffer);
        assertEq(original.lastUpdated, decoded.lastUpdated);
    }

    function test_Pool_Member_Data_Encode_Decode(
        address pool,
        uint32 poolId
    ) public {
        vm.assume(pool != address(0));
        GeneralDistributionAgreementV1.PoolMemberData
            memory original = GeneralDistributionAgreementV1.PoolMemberData({
                pool: pool,
                poolId: poolId
            });
        bytes32[] memory encodedData = _encodePoolMemberData(original);
        (
            ,
            GeneralDistributionAgreementV1.PoolMemberData memory decoded
        ) = _decodePoolMemberData(uint256(encodedData[0]));

        assertEq(original.pool, decoded.pool);
        assertEq(original.poolId, decoded.poolId);
    }
}
