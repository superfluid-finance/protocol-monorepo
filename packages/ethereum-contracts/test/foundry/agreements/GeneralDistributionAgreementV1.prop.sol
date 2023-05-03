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

/// @title GeneralDistributionAgreementV1 Property Tests
/// @author Superfluid
/// @notice This is a contract that runs property tests for the GDAv1
/// It involves testing the pure functions of the GDAv1 to ensure that we get
/// the expected output for a range of inputs.
contract GeneralDistributionAgreementV1Properties is
    GeneralDistributionAgreementV1,
    Test
{
    constructor() GeneralDistributionAgreementV1(ISuperfluid(address(0))) {}

    function testParticleInputEncodeDecodeUniversalIndexData(
        int96 flowRate,
        uint32 settledAt,
        int256 settledValue,
        uint96 totalBuffer,
        bool isPool_
    ) public {
        BasicParticle memory particle = BasicParticle({
            _flow_rate: FlowRate.wrap(flowRate),
            _settled_at: Time.wrap(settledAt),
            _settled_value: Value.wrap(settledValue)
        });
        bytes32[] memory encoded = _encodeUniversalIndexData(
            particle,
            totalBuffer,
            isPool_
        );
        (, UniversalIndexData memory decoded) = _decodeUniversalIndexData(
            encoded
        );

        assertEq(flowRate, decoded.flowRate, "flowRate not equal");
        assertEq(settledAt, decoded.settledAt, "settledAt not equal");
        assertEq(settledValue, decoded.settledValue, "settledValue not equal");
        assertEq(totalBuffer, decoded.totalBuffer, "totalBuffer not equal");
        assertEq(isPool_, decoded.isPool, "isPool not equal");
    }

    function testUIDataInputEncodeDecodeUniversalIndexData(
        int96 flowRate,
        uint32 settledAt,
        int256 settledValue,
        uint96 totalBuffer,
        bool isPool_
    ) public {
        UniversalIndexData memory data = UniversalIndexData({
            flowRate: flowRate,
            settledAt: settledAt,
            settledValue: settledValue,
            totalBuffer: totalBuffer,
            isPool: isPool_
        });

        bytes32[] memory encoded = _encodeUniversalIndexData(data);
        (, UniversalIndexData memory decoded) = _decodeUniversalIndexData(
            encoded
        );

        assertEq(flowRate, decoded.flowRate, "flowRate not equal");
        assertEq(settledAt, decoded.settledAt, "settledAt not equal");
        assertEq(settledValue, decoded.settledValue, "settledValue not equal");
        assertEq(totalBuffer, decoded.totalBuffer, "totalBuffer not equal");
        assertEq(isPool_, decoded.isPool, "isPool not equal");
    }

    function testGetBasicParticleFromUIndex(
        UniversalIndexData memory data
    ) public {
        BasicParticle memory particle = _getBasicParticleFromUIndex(data);
        assertEq(data.flowRate, int96(FlowRate.unwrap(particle._flow_rate)), "flowRate not equal");
        assertEq(data.settledAt, Time.unwrap(particle._settled_at), "settledAt not equal");
        assertEq(data.settledValue, Value.unwrap(particle._settled_value), "settledValue not equal");
    }

    function testFlowDistributionDataEncodeDecode(
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

    function testPoolMemberDataEncodeDecode(
        address pool,
        uint32 poolID
    ) public {
        vm.assume(pool != address(0));
        GeneralDistributionAgreementV1.PoolMemberData
            memory original = GeneralDistributionAgreementV1.PoolMemberData({
                pool: pool,
                poolID: poolID
            });
        bytes32[] memory encoded = _encodePoolMemberData(original);
        (
            ,
            GeneralDistributionAgreementV1.PoolMemberData memory decoded
        ) = _decodePoolMemberData(uint256(encoded[0]));

        assertEq(original.pool, decoded.pool, "pool not equal");
        assertEq(original.poolID, decoded.poolID, "poolID not equal");
    }
}