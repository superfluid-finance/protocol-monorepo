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
 *       Its naming is inspired by effect systems.
 */
abstract contract TokenMonad {
    function _getUIndex(bytes memory eff, address owner)
        virtual internal view returns (BasicParticle memory);
    function _setUIndex(bytes memory eff, address owner, BasicParticle memory p)
        virtual internal returns (bytes memory);
    function _getPDPIndex(bytes memory eff, address pool)
        virtual internal view returns (PDPoolIndex memory);
    function _setPDPIndex(bytes memory eff, address pool, PDPoolIndex memory p)
        virtual internal returns (bytes memory);
    function _getFlowRate(bytes memory, bytes32 flowHash)
        virtual internal view returns (FlowRate);
    function _setFlowInfo(bytes memory eff, bytes32 flowHash, address from, address to,
                          FlowRate newFlowRate, FlowRate flowRateDelta)
        virtual internal returns (bytes memory);
    function _getPoolAdjustmentFlowInfo(bytes memory eff, address pool)
        virtual internal view returns (address recipient, bytes32 flowHash, FlowRate flowRate);

    function _doShift(bytes memory eff, address from, address to, Value amount)
        internal returns (bytes memory)
    {
        if (from == to) return eff; // short circuit
        BasicParticle memory a = _getUIndex(eff, from);
        BasicParticle memory b = _getUIndex(eff, to);

        (a, b) = a.shift2(b, amount);

        eff = _setUIndex(eff, from, a);
        eff = _setUIndex(eff, to, b);

        return eff;
    }

    function _doFlow(bytes memory eff, address from, address to, bytes32 flowHash, FlowRate flowRate, Time t)
        internal returns (bytes memory)
    {
        if (from == to) return eff; // short circuit
        FlowRate flowRateDelta = flowRate - _getFlowRate(eff, flowHash);
        BasicParticle memory a = _getUIndex(eff, from);
        BasicParticle memory b = _getUIndex(eff, to);

        (a, b) = a.shift_flow2b(b, flowRateDelta, t);

        eff = _setUIndex(eff, from, a);
        eff = _setUIndex(eff, to, b);
        eff = _setFlowInfo(eff, flowHash, from, to, flowRate, flowRateDelta);

        return eff;
    }

    function _doDistribute(bytes memory eff, address from, address pool, Value reqAmount)
        internal returns (bytes memory, Value actualAmount)
    {
        assert(from != pool);
        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, pool);

        (a, pdpIndex, actualAmount) = a.shift2b(pdpIndex, reqAmount);

        eff = _setUIndex(eff, from, a);
        eff = _setPDPIndex(eff, pool, pdpIndex);

        return (eff, actualAmount);
    }

    // Note: because of no-via-ir builds and stack too deep :)
    struct _DistributeFlowVars {
        address adjustmentRecipient;
        bytes32 adjustmentFlowHash;
        FlowRate adjustmentFlowRate;
        FlowRate actualFlowRateDelta;
    }
    function _doDistributeFlow(bytes memory eff,
                               address from, address pool, bytes32 flowHash, FlowRate reqFlowRate,
                               Time t)
        internal returns (bytes memory, FlowRate actualFlowRate, FlowRate newDistributionFlowRate)
    {
        assert(from != pool);
        _DistributeFlowVars memory vars;

        BasicParticle memory a = _getUIndex(eff, from);
        PDPoolIndex memory pdpIndex = _getPDPIndex(eff, pool);
        (vars.adjustmentRecipient, vars.adjustmentFlowHash, vars.adjustmentFlowRate) =
            _getPoolAdjustmentFlowInfo(eff, pool);

        {
            FlowRate oldFlowRate = _getFlowRate(eff, flowHash);
            FlowRate oldDistributionFlowRate = pdpIndex.flow_rate();
            // include the adjustment flow rate too, as part of the readjustment process
            FlowRate shiftFlowRate = reqFlowRate + vars.adjustmentFlowRate - oldFlowRate;

            (a, pdpIndex, newDistributionFlowRate) = a.shift_flow2b(pdpIndex, shiftFlowRate, t);
            assert(FlowRate.unwrap(newDistributionFlowRate) >= 0);
            vars.actualFlowRateDelta = newDistributionFlowRate - oldDistributionFlowRate;
            actualFlowRate = oldFlowRate + vars.actualFlowRateDelta - vars.adjustmentFlowRate;

            if (FlowRate.unwrap(actualFlowRate) > 0) {
                // previous adjustment flow is fully utilized
                vars.adjustmentFlowRate = FlowRate.wrap(0);
            } else {
                // previous adjustment flow still needed
                vars.adjustmentFlowRate = actualFlowRate.inv();
                actualFlowRate = FlowRate.wrap(0);
            }
        }

        eff = _setUIndex(eff, from, a);
        eff = _setPDPIndex(eff, pool, pdpIndex);
        eff = _setFlowInfo(eff, flowHash, from, pool, actualFlowRate, vars.actualFlowRateDelta);
        eff = _doFlow(eff, pool, vars.adjustmentRecipient, vars.adjustmentFlowHash, vars.adjustmentFlowRate, t);

        return (eff, actualFlowRate, newDistributionFlowRate);
    }
}
