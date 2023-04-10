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
        internal virtual returns (bytes memory);
    function _getPDPIndex(bytes memory eff, address pool)
        internal view virtual returns (PDPoolIndex memory);
    function _setPDPIndex(bytes memory eff, address pool, PDPoolIndex memory p)
        internal virtual returns (bytes memory);
    function _getFlowRate(bytes memory, bytes32 flowHash)
        internal view virtual returns (FlowRate);
    function _setFlowInfo(bytes memory eff, bytes32 flowHash, address from, address to, FlowRate flowRate)
        virtual internal returns (bytes memory);

    function _doShift(bytes memory eff, address from, address to, Value amount)
        internal returns (bytes memory)
    {
        BasicParticle memory mempty;
        (BasicParticle memory a, BasicParticle memory b) = mempty.shift2(mempty, amount);
        // using mappend so that it works when from == to, why not
        eff = _setUIndex(eff, from, _getUIndex(eff, from).mappend(a));
        eff = _setUIndex(eff, to, _getUIndex(eff, to).mappend(b));
        return eff;
    }

    function _doFlow(bytes memory eff, address from, address to, bytes32 flowHash, FlowRate flowRate, Time t)
        internal returns (bytes memory)
    {
        FlowRate flowRateDelta = flowRate - _getFlowRate(eff, flowHash);
        BasicParticle memory a = _getUIndex(eff, from);
        BasicParticle memory b = _getUIndex(eff, to);
        (a, b) = a.shift_flow2b(b, flowRateDelta, t);
        eff = _setUIndex(eff, from, a);
        eff = _setUIndex(eff, to, b);
        eff = _setFlowInfo(eff, flowHash, from, to, flowRate);
        return eff;
    }

    function _doDistribute(bytes memory eff, address from, address to, Value reqAmount)
        internal returns (bytes memory, Value actualAmount)
    {
        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, to);
        (a, pdpIndex, actualAmount) = a.shift2b(pdpIndex, reqAmount);
        eff = _setUIndex(eff, from, a);
        eff = _setPDPIndex(eff, to, pdpIndex);
        return (eff, actualAmount);
    }

    function _doDistributeFlow(bytes memory eff,
                               address from, address to, bytes32 flowHash, FlowRate reqFlowRate, Time t)
        internal returns (bytes memory, FlowRate actualFlowRate)
    {
        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, to);
        FlowRate flowRateDelta = reqFlowRate - _getFlowRate(eff, flowHash);
        (a, pdpIndex, actualFlowRate) = a.shift_flow2b(pdpIndex, flowRateDelta, t);
        eff = _setUIndex(eff, from, a);
        eff = _setPDPIndex(eff, to, pdpIndex);
        eff = _setFlowInfo(eff, flowHash, from, to, actualFlowRate);
        return (eff, actualFlowRate);
    }
}
