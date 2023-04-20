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
    constructor() GeneralDistributionAgreementV1(ISuperfluid(address(0))) {}
}

contract GeneralDistributionAgreementV1Properties is
    GeneralDistributionAgreementV1Mock,
    Test
{
    function test_Particle_Input_Encode_Decode_Universal_Index_Data(
        int96 flowRate,
        uint32 settledAt,
        int256 settledValue,
        uint96 totalBuffer,
        bool isPool
    ) public {
        BasicParticle memory particle = BasicParticle({
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_at: Time.wrap(settledAt),
            _settled_value: Value.wrap(settledValue)
        });
        bytes32[] memory encoded = _encodeUniversalIndexData(
            particle,
            totalBuffer,
            isPool
        );
        (, UniversalIndexData memory decoded) = _decodeUniversalIndexData(
            encoded
        );

        assertEq(flowRate, decoded.flowRate, "flowRate not equal");
        assertEq(settledAt, decoded.settledAt, "settledAt not equal");
        assertEq(settledValue, decoded.settledValue, "settledValue not equal");
        assertEq(totalBuffer, decoded.totalBuffer, "totalBuffer not equal");
        assertEq(isPool, decoded.isPool, "isPool not equal");
    }

    function test_UIData_Input_Encode_Decode_Universal_Index_Data(
        int96 flowRate,
        uint32 settledAt,
        int256 settledValue,
        uint96 totalBuffer,
        bool isPool
    ) public {
        UniversalIndexData memory data = UniversalIndexData({
            flowRate: flowRate,
            settledAt: settledAt,
            settledValue: settledValue,
            totalBuffer: totalBuffer,
            isPool: isPool
        });

        bytes32[] memory encoded = _encodeUniversalIndexData(data);
        (, UniversalIndexData memory decoded) = _decodeUniversalIndexData(
            encoded
        );

        assertEq(flowRate, decoded.flowRate, "flowRate not equal");
        assertEq(settledAt, decoded.settledAt, "settledAt not equal");
        assertEq(settledValue, decoded.settledValue, "settledValue not equal");
        assertEq(totalBuffer, decoded.totalBuffer, "totalBuffer not equal");
        assertEq(isPool, decoded.isPool, "isPool not equal");
    }

    function test_Get_Basic_Particle_From_UIndex(UniversalIndexData memory data) public {
        BasicParticle memory particle = _getBasicParticleFromUIndex(data);
        assertEq(data.flowRate, int96(FlowRate.unwrap(particle._flow_rate)), "flowRate not equal");
        assertEq(data.settledAt, Time.unwrap(particle._settled_at), "settledAt not equal");
        assertEq(data.settledValue, Value.unwrap(particle._settled_value), "settledValue not equal");
    }

    function test_Flow_Distribution_Data_Encode_Decode(
        int96 flowRate,
        uint96 buffer
    ) public {
        vm.assume(flowRate >= 0);
        vm.assume(buffer >= 0);
        GeneralDistributionAgreementV1.FlowDistributionData
            memory original = GeneralDistributionAgreementV1
                .FlowDistributionData({
                    flowRate: flowRate,
                    lastUpdated: uint32(block.timestamp),
                    buffer: buffer
                });
        bytes32[] memory encoded = _encodeFlowDistributionData(original);
        (
            ,
            GeneralDistributionAgreementV1.FlowDistributionData memory decoded
        ) = _decodeFlowDistributionData(uint256(encoded[0]));

        assertEq(original.flowRate, decoded.flowRate, "flowRate not equal");
        assertEq(original.buffer, decoded.buffer, "buffer not equal");
        assertEq(original.lastUpdated, decoded.lastUpdated, "lastUpdated not equal");
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
        bytes32[] memory encoded = _encodePoolMemberData(original);
        (
            ,
            GeneralDistributionAgreementV1.PoolMemberData memory decoded
        ) = _decodePoolMemberData(uint256(encoded[0]));

        assertEq(original.pool, decoded.pool, "pool not equal");
        assertEq(original.poolId, decoded.poolId, "poolId not equal");
    }
}
