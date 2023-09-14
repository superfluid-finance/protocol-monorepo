// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";

import {
    Time,
    Value,
    FlowRate,
    BasicParticle,
    PDPoolIndex
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import { TokenMonad } from "@superfluid-finance/solidity-semantic-money/src/TokenMonad.sol";

import { SuperfluidPool } from "../superfluid/SuperfluidPool.sol";
import { ISuperfluidPool } from "../interfaces/superfluid/ISuperfluidPool.sol";
import { GeneralDistributionAgreementV1 } from "./GeneralDistributionAgreementV1.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";

/**
 * @title DelegatableTokenMonad
 * A version of the TokenMonad contract which is deployed and called by the GDA
 * due to issues with code size.
 */
contract DelegatableTokenMonad is TokenMonad {
    using SafeCast for int256;
    /// NOTE - Duplicated
    /// @dev Universal Index state slot id for storing universal index data

    uint256 private constant _UNIVERSAL_INDEX_STATE_SLOT_ID = 0;

    /// NOTE - Duplicated in GDAv1
    function _getBasicParticleFromUIndex(GeneralDistributionAgreementV1.UniversalIndexData memory universalIndexData)
        internal
        pure
        returns (BasicParticle memory particle)
    {
        particle._flow_rate = FlowRate.wrap(universalIndexData.flowRate);
        particle._settled_at = Time.wrap(universalIndexData.settledAt);
        particle._settled_value = Value.wrap(universalIndexData.settledValue);
    }

    /// NOTE - Duplicated in GDAv1
    function _decodeUniversalIndexData(bytes32[] memory data)
        internal
        pure
        returns (bool exists, GeneralDistributionAgreementV1.UniversalIndexData memory universalIndexData)
    {
        uint256 a = uint256(data[0]);
        uint256 b = uint256(data[1]);

        exists = a > 0 || b > 0;

        if (exists) {
            universalIndexData.flowRate = int96(int256(a >> 160) & int256(uint256(type(uint96).max)));
            universalIndexData.settledAt = uint32(uint256(a >> 128) & uint256(type(uint32).max));
            universalIndexData.totalBuffer = uint256(a >> 32) & uint256(type(uint96).max);
            universalIndexData.isPool = ((a << 224) >> 224) & 1 == 1;
            universalIndexData.settledValue = int256(b);
        }
    }

    /// NOTE - Duplicated in GDAv1
    function _getUIndexData(bytes memory eff, address owner)
        internal
        view
        returns (GeneralDistributionAgreementV1.UniversalIndexData memory universalIndexData)
    {
        (, universalIndexData) = _decodeUniversalIndexData(
            ISuperfluidToken(abi.decode(eff, (address))).getAgreementStateSlot(
                address(this), owner, _UNIVERSAL_INDEX_STATE_SLOT_ID, 2
            )
        );
    }

    function _encodeUniversalIndexData(BasicParticle memory p, uint256 buffer, bool isPool_)
        internal
        pure
        returns (bytes32[] memory data)
    {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(FlowRate.unwrap(p.flow_rate()))) << 160) | (uint256(Time.unwrap(p.settled_at())) << 128)
                | (buffer << 32) | (isPool_ ? 1 : 0)
        );
        data[1] = bytes32(uint256(Value.unwrap(p._settled_value)));
    }

    function _encodeUniversalIndexData(GeneralDistributionAgreementV1.UniversalIndexData memory uIndexData)
        internal
        pure
        returns (bytes32[] memory data)
    {
        data = new bytes32[](2);
        data[0] = bytes32(
            (uint256(int256(uIndexData.flowRate)) << 160) | (uint256(uIndexData.settledAt) << 128)
                | (uint256(uIndexData.totalBuffer) << 32) | (uIndexData.isPool ? 1 : 0)
        );
        data[1] = bytes32(uint256(uIndexData.settledValue));
    }

    function _encodeFlowDistributionData(
        GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData
    ) internal pure returns (bytes32[] memory data) {
        data = new bytes32[](1);
        data[0] = bytes32(
            (uint256(uint32(flowDistributionData.lastUpdated)) << 192)
                | (uint256(uint96(flowDistributionData.flowRate)) << 96) | uint256(flowDistributionData.buffer)
        );
    }

    function _decodeFlowDistributionData(uint256 data)
        internal
        pure
        returns (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData)
    {
        exist = data > 0;
        if (exist) {
            flowDistributionData.lastUpdated = uint32((data >> 192) & uint256(type(uint32).max));
            flowDistributionData.flowRate = int96(int256(data >> 96));
            flowDistributionData.buffer = uint96(data & uint256(type(uint96).max));
        }
    }

    function _getFlowDistributionData(ISuperfluidToken token, bytes32 distributionFlowHash)
        internal
        view
        returns (bool exist, GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData)
    {
        (exist, flowDistributionData) =
            _decodeFlowDistributionData(uint256(token.getAgreementData(address(this), distributionFlowHash, 1)[0]));
    }

    function _getPoolAdjustmentFlowInfo(bytes memory eff, address pool)
        internal
        view
        returns (address adjustmentRecipient, bytes32 flowHash, int96 flowRate)
    {
        // pool admin is always the adjustment recipient
        adjustmentRecipient = ISuperfluidPool(pool).admin();
        flowHash = _getPoolAdjustmentFlowHash(pool, adjustmentRecipient);
        return (adjustmentRecipient, flowHash, int256(FlowRate.unwrap(_getFlowRate(eff, flowHash))).toInt96());
    }

    /// NOTE - Duplicated in GDAv1
    function _getPoolAdjustmentFlowHash(address from, address to) internal view returns (bytes32) {
        // this will never be in conflict with other flow has types
        return keccak256(abi.encode(block.chainid, "poolAdjustmentFlow", from, to));
    }

    /// NOTE - Duplicated in GDAv1
    function _getUIndex(bytes memory eff, address owner) internal view override returns (BasicParticle memory uIndex) {
        (, GeneralDistributionAgreementV1.UniversalIndexData memory universalIndexData) = _decodeUniversalIndexData(
            ISuperfluidToken(abi.decode(eff, (address))).getAgreementStateSlot(
                address(this), owner, _UNIVERSAL_INDEX_STATE_SLOT_ID, 2
            )
        );
        uIndex = _getBasicParticleFromUIndex(universalIndexData);
    }

    function setUIndex(bytes memory eff, address owner, BasicParticle memory p) external returns (bytes memory) {
        return _setUIndex(eff, owner, p);
    }

    function _setUIndex(bytes memory eff, address owner, BasicParticle memory p)
        internal
        override
        returns (bytes memory)
    {
        GeneralDistributionAgreementV1.UniversalIndexData memory universalIndexData = _getUIndexData(eff, owner);

        ISuperfluidToken(abi.decode(eff, (address))).updateAgreementStateSlot(
            owner,
            _UNIVERSAL_INDEX_STATE_SLOT_ID,
            _encodeUniversalIndexData(p, universalIndexData.totalBuffer, universalIndexData.isPool)
        );

        return eff;
    }

    /// NOTE - Duplicated in GDAv1
    function _getPDPIndex(
        bytes memory, // eff,
        address pool
    ) internal view override returns (PDPoolIndex memory) {
        SuperfluidPool.PoolIndexData memory data = SuperfluidPool(pool).getIndex();
        return SuperfluidPool(pool).poolIndexDataToPDPoolIndex(data);
    }

    function _setPDPIndex(bytes memory eff, address pool, PDPoolIndex memory p)
        internal
        override
        returns (bytes memory)
    {
        assert(SuperfluidPool(pool).operatorSetIndex(p));

        return eff;
    }

    /// NOTE - Duplicated in GDAv1
    function _getFlowRate(bytes memory eff, bytes32 distributionFlowHash) internal view override returns (FlowRate) {
        (, GeneralDistributionAgreementV1.FlowDistributionData memory data) =
            _getFlowDistributionData(ISuperfluidToken(abi.decode(eff, (address))), distributionFlowHash);
        return FlowRate.wrap(data.flowRate);
    }

    function _setFlowInfo(
        bytes memory eff,
        bytes32 flowHash,
        address, // from,
        address, // to,
        FlowRate newFlowRate,
        FlowRate // flowRateDelta
    ) internal override returns (bytes memory) {
        address token = abi.decode(eff, (address));
        (, GeneralDistributionAgreementV1.FlowDistributionData memory flowDistributionData) =
            _getFlowDistributionData(ISuperfluidToken(token), flowHash);

        ISuperfluidToken(token).updateAgreementData(
            flowHash,
            _encodeFlowDistributionData(
                GeneralDistributionAgreementV1.FlowDistributionData({
                    lastUpdated: uint32(block.timestamp),
                    flowRate: int256(FlowRate.unwrap(newFlowRate)).toInt96(),
                    buffer: flowDistributionData.buffer
                })
            )
        );

        return eff;
    }

    function _getPoolAdjustmentFlowRate(bytes memory eff, address pool)
        internal
        view
        override
        returns (FlowRate flowRate)
    {
        (,, int96 rawFlowRate) = _getPoolAdjustmentFlowInfo(eff, pool);
        flowRate = FlowRate.wrap(int128(rawFlowRate)); // upcasting to int128 is safe
    }

    /// NOTE - Duplicated in GDAv1
    function _setPoolAdjustmentFlowRate(bytes memory eff, address pool, bool doShiftFlow, FlowRate flowRate, Time t)
        internal
        returns (bytes memory)
    {
        // @note should this also always be
        address adjustmentRecipient = ISuperfluidPool(pool).admin();
        bytes32 adjustmentFlowHash = _getPoolAdjustmentFlowHash(pool, adjustmentRecipient);

        if (doShiftFlow) {
            flowRate = flowRate + _getFlowRate(eff, adjustmentFlowHash);
        }
        eff = _doFlow(eff, pool, adjustmentRecipient, adjustmentFlowHash, flowRate, t);
        return eff;
    }

    function _setPoolAdjustmentFlowRate(bytes memory eff, address pool, FlowRate flowRate, Time t)
        internal
        override
        returns (bytes memory)
    {
        return _setPoolAdjustmentFlowRate(eff, pool, false, /* doShift? */ flowRate, t);
    }

    function doShift(bytes memory eff, address from, address to, Value amount) external returns (bytes memory) {
        return _doShift(eff, from, to, amount);
    }

    function doFlow(bytes memory eff, address from, address to, bytes32 flowHash, FlowRate flowRate, Time t)
        external
        returns (bytes memory)
    {
        return _doFlow(eff, from, to, flowHash, flowRate, t);
    }

    function doDistributeViaPool(bytes memory eff, address from, address pool, Value reqAmount)
        external
        returns (bytes memory, Value actualAmount)
    {
        return _doDistributeViaPool(eff, from, pool, reqAmount);
    }

    function doDistributeFlowViaPool(
        bytes memory eff,
        address from,
        address pool,
        bytes32 flowHash,
        FlowRate reqFlowRate,
        Time t
    ) external returns (bytes memory, FlowRate newActualFlowRate, FlowRate newDistributionFlowRate) {
        return _doDistributeFlowViaPool(eff, from, pool, flowHash, reqFlowRate, t);
    }
}
