// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;


import {
    Time, FlowRate, Value, Unit
} from "@superfluid-finance/solidity-semantic-money/src/SemanticMoney.sol";
import {
    FlowId, ToySuperfluidToken
} from "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperfluidToken.sol";
import {
    ToySuperfluidPool
} from "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperfluidPool.sol";


/**
 * @dev Aqueduct - a zero-intermediate liquidity market maker (ZILMM)
 */
library AqueductLibrary {
    struct SideState {
        FlowRate totalInFlowRate;
    }

    function clone(SideState memory a) internal pure returns (SideState memory b) {
        b.totalInFlowRate = a.totalInFlowRate;
    }

    function updateSide(SideState memory a, Unit curUnitsB, FlowRate r0, FlowRate r1) internal pure
        returns (SideState memory m, FlowRate newDistFlowRateA, Unit newUnitsB)
    {
        m = clone(a);
        m.totalInFlowRate = a.totalInFlowRate + r1 - r0;

        // TODO: assuming 100% distributed for now
        newDistFlowRateA = m.totalInFlowRate;

        newUnitsB = Unit.wrap(Unit.unwrap(curUnitsB) + FlowRate.unwrap(r1) - FlowRate.unwrap(r0));
    }
}

contract Aqueduct {
    using AqueductLibrary for AqueductLibrary.SideState;

    FlowId constant public SWAP_DISTRIBUTE_FLOW_ID = FlowId.wrap(0);
    FlowId constant public ADJUSTMENT_FLOW_ID = FlowId.wrap(0);

    struct Side {
        ToySuperfluidToken token;
        ToySuperfluidPool pool;
        AqueductLibrary.SideState state;
        address remFlowReceiver;
    }

    Side internal _left;
    Side internal _right;

    function token1() external view returns (ToySuperfluidToken) { return _left.token; }
    function token2() external view returns (ToySuperfluidToken) { return _right.token; }
    function pool1() external view returns (ToySuperfluidPool) { return _left.pool; }
    function pool2() external view returns (ToySuperfluidPool) { return _right.pool; }

    constructor (ToySuperfluidToken tokenL, ToySuperfluidToken tokenR) {
        _left.token = tokenL;
        _left.pool = tokenL.createPool();
        _right.token = tokenR;
        _right.pool = tokenR.createPool();
    }

    function onFlowUpdate(ToySuperfluidToken token, address from, FlowRate ir0, FlowRate ir1) external {
        if (token == _left.token) {
            _onFlowUpdate(_left, _right, from, ir0, ir1);
        } else if (token == _right.token) {
            _onFlowUpdate(_right, _left, from, ir0, ir1);
        } else revert("Unknown token");
    }

    function _onFlowUpdate(Side storage a, Side storage b, address from, FlowRate ir0, FlowRate ir1) internal {
        (AqueductLibrary.SideState memory sa1, FlowRate drr, Unit u1) =
            a.state.updateSide(b.pool.getUnits(from), ir0, ir1);
        a.state = sa1;

        {
            FlowRate ar0 = a.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
            FlowRate dr0 = a.pool.getDistributionFlowRate();
            _adjustFlowRemainder(a, from, ir1 - ir0, drr, ar0, dr0);
        }

        {
            FlowRate ar0 = b.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
            FlowRate dr0 = b.pool.getDistributionFlowRate();
            b.pool.updateMember(from, u1);
            _adjustFlowRemainder(b, from, FlowRate.wrap(0), dr0 + ar0, ar0, dr0);
        }
    }

    function _adjustFlowRemainder(Side storage a, address from,
                                  FlowRate irr, FlowRate drr,
                                  FlowRate ar0, FlowRate dr0) internal {
        (,FlowRate dr1) = a.token.distributeFlow(address(this), a.pool, SWAP_DISTRIBUTE_FLOW_ID, drr);

        if (a.remFlowReceiver != from) {
            FlowRate br0 = a.token.getFlowRate(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID);
            // solve ar1 in: irr = (dr1 - dr0) + (ar1 - ar0) + (0 - br0)
            // =>
            FlowRate ar1 = irr + dr0 - dr1 + ar0 + br0;

            a.token.flow(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID, FlowRate.wrap(0));
            a.token.flow(address(this), from, ADJUSTMENT_FLOW_ID, ar1);
            a.remFlowReceiver = from;
        } else {
            // solve ar1 in: irr = (dr1 - dr0) + (ar1 - ar0)
            // =>
            FlowRate ar1 = irr + dr0 - dr1 + ar0;

            a.token.flow(address(this), from, ADJUSTMENT_FLOW_ID, ar1);
        }
    }
}
