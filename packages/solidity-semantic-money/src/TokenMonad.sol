// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.19;

import {
    Time, Value, FlowRate, Unit,
    BasicParticle,
    PDPoolIndex, PDPoolMember, PDPoolMemberMU
} from "./SemanticMoney.sol";

/**
 * @title Monadic interface for Semantic Money as abstract contract.
 *
 * Note:
 *
 * eff - Opaque context for all monadic effects. They should be used to provide the context for the getter/setters.
 *       Its naming is inspired by the effect systems.
 */
abstract contract TokenMonad {
    function _getUIndex(bytes memory eff, address owner)
        internal view virtual returns (BasicParticle memory);
    function _setUIndex(bytes memory eff, address owner, BasicParticle memory p)
        internal virtual;
    function _getPDPIndex(bytes memory eff, address pool)
        internal view virtual returns (PDPoolIndex memory);
    function _setPDPIndex(bytes memory eff, address pool, PDPoolIndex memory p)
        internal virtual;
    function _getFlowRate(bytes memory eff, bytes32 flowHash)
        internal view virtual returns (FlowRate);
    function _setFlowInfo(bytes memory eff, bytes32 flowHash, address from, address to, FlowRate flowRate)
        virtual internal;

    function _doShift(bytes memory eff, address from, address to, Value amount)
        internal
    {
        BasicParticle memory mempty;
        (BasicParticle memory a, BasicParticle memory b) = mempty.shift2(mempty, amount);
        // using mappend so that it works when from == to, why not
        _setUIndex(eff, from, _getUIndex(eff, from).mappend(a));
        _setUIndex(eff, to, _getUIndex(eff, to).mappend(b));
    }

    function _doFlow(bytes memory eff, address from, address to, bytes32 flowHash, FlowRate flowRate, Time t)
        internal
    {
        BasicParticle memory mempty;
        FlowRate flowRateDelta = flowRate - _getFlowRate(eff, flowHash);
        (BasicParticle memory a, BasicParticle memory b) = mempty.flow2(mempty, flowRateDelta, t);
        // using mappend so that it works when from == to, why not
        _setUIndex(eff, from, _getUIndex(eff, from).mappend(a));
        _setUIndex(eff, to, _getUIndex(eff, to).mappend(b));
        _setFlowInfo(eff, flowHash, from, to, flowRate);
    }

    function _doDistribute(bytes memory eff, address from, address to, Value reqAmount)
        internal returns (Value actualAmount)
    {
        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, to);
        (a, pdpIndex, actualAmount) = a.shift2(pdpIndex, reqAmount);
        _setUIndex(eff, from, a);
        _setPDPIndex(eff, to, pdpIndex);
    }

    function _doDistributeFlow(bytes memory eff,
                               address from, address to, bytes32 flowHash, FlowRate reqFlowRate, Time t)
        internal returns (FlowRate actualFlowRate)
    {
        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, to);
        FlowRate flowRateDelta = reqFlowRate - _getFlowRate(eff, flowHash);
        (a, pdpIndex, actualFlowRate) = a.shift_flow2b(pdpIndex, flowRateDelta, t);
        _setUIndex(eff, from, a);
        _setPDPIndex(eff, to, pdpIndex);
        _setFlowInfo(eff, flowHash, from, to, actualFlowRate);
    }
}
