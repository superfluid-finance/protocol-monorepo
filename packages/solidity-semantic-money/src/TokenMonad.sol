// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.19;

import {
    Time, Value, FlowRate, Unit,
    BasicParticle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "./SemanticMoney.sol";

/**
 * @title Monadic interface for Semantic Money.
 *
 * Note:
 *
 * Because of lacking closure or context for the internal callbacks, it has serious
 * limitations in making it useful in some cases.
 */
abstract contract TokenMonad {
    function _getUIndex(address owner) internal view virtual returns (BasicParticle memory);
    function _setUIndex(address owner, BasicParticle memory p) internal virtual;
    function _getPDPIndex(address pool) internal view virtual returns (PDPoolIndex memory);
    function _setPDPIndex(address pool, PDPoolIndex memory p) internal virtual;
    function _getFlowRate(bytes32 flowHash) internal view virtual returns (FlowRate);
    function _setFlowInfo(bytes32 flowHash, address from, address to, FlowRate flowRate) virtual internal;

    function _doShift(address from, address to, Value amount) internal {
        BasicParticle memory mempty;
        (BasicParticle memory a, BasicParticle memory b) = mempty.shift2(mempty, amount);
        // using mappend so that it works when from == to, why not
        _setUIndex(from, _getUIndex(from).mappend(a));
        _setUIndex(to, _getUIndex(to).mappend(b));
    }

    function _doFlow(address from, address to, bytes32 flowHash, FlowRate flowRate, Time t) internal {
        BasicParticle memory mempty;
        FlowRate flowRateDelta = flowRate - _getFlowRate(flowHash);
        (BasicParticle memory a, BasicParticle memory b) = mempty.shift_flow2a(mempty, flowRateDelta, t);
        // using mappend so that it works when from == to, why not
        _setUIndex(from, _getUIndex(from).mappend(a));
        _setUIndex(to, _getUIndex(to).mappend(b));
        _setFlowInfo(flowHash, from, to, flowRate);
    }

    function _doDistribute(address from, address to, Value reqAmount) internal returns (Value actualAmount) {
        BasicParticle memory a = _getUIndex(from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(to);
        (a, pdpIndex, actualAmount) = a.shift2(pdpIndex, reqAmount);
        _setUIndex(from, a);
        _setPDPIndex(to, pdpIndex);
    }

    function _doDistributeFlow(address from, address to, bytes32 flowHash, FlowRate reqFlowRate, Time t) internal
        returns (FlowRate actualFlowRate)
    {
        BasicParticle memory a = _getUIndex(from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(to);
        FlowRate flowRateDelta = reqFlowRate - _getFlowRate(flowHash);
        (a, pdpIndex, actualFlowRate) = a.shift_flow2b(pdpIndex, flowRateDelta, t);
        _setUIndex(from, a);
        _setPDPIndex(to, pdpIndex);
        _setFlowInfo(flowHash, from, to, actualFlowRate);
    }
}
