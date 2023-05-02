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
    int128 internal constant FLOW_RATE_SCALING_FACTOR = 1e9;

    struct PoolSideState {
        FlowRate totalInFlowRate;
    }

    struct TraderSideState {
        Unit nUnits;
    }

    // Down scale the flow rate: minimum 1 unit unless input is 0.
    function _clip(FlowRate r) internal pure returns(int128)
    {
        if (FlowRate.unwrap(r) == 0) return 0;
        int128 rr = FlowRate.unwrap(r) / FLOW_RATE_SCALING_FACTOR;
        return rr == 0 ? int128(1) : rr;
    }

    function updateSide(PoolSideState memory poolA, FlowRate r0, FlowRate r1) internal pure
        returns (PoolSideState memory poolAu, TraderSideState memory tradeBu, FlowRate distributionFlowRate)
    {
        poolAu.totalInFlowRate = poolA.totalInFlowRate + r1 - r0;
        // TODO: assuming 100% distributed for now
        distributionFlowRate = poolAu.totalInFlowRate;

        tradeBu.nUnits = Unit.wrap(_clip(r1));
    }
}

contract Aqueduct {
    FlowId constant public SWAP_DISTRIBUTE_FLOW_ID = FlowId.wrap(0);
    FlowId constant public ADJUSTMENT_FLOW_ID = FlowId.wrap(0);

    struct Side {
        ToySuperfluidToken token;
        ToySuperfluidPool pool;
        // it is unnecessary to store everything in storage, but it's a toy model
        AqueductLibrary.PoolSideState state;
        // remainder flowrate goes to the last taker
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
        AqueductLibrary.TraderSideState memory tssB;
        FlowRate drr;
        (a.state, tssB, drr) = AqueductLibrary.updateSide(a.state, ir0, ir1);

        {
            FlowRate ar0 = a.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
            FlowRate dr0 = a.pool.getConnectedFlowRate();
            (,,FlowRate dr1) = a.token.distributeFlow(address(this), a.pool, SWAP_DISTRIBUTE_FLOW_ID, drr);
            _adjustFlowRemainder(a, from, ir1 - ir0, dr1, ar0, dr0);
        }

        {
            FlowRate ar0 = b.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
            FlowRate dr0 = b.pool.getConnectedFlowRate();
            b.pool.updateMember(from, tssB.nUnits);
            (,,FlowRate dr1) = b.token.distributeFlow(address(this), b.pool, SWAP_DISTRIBUTE_FLOW_ID, dr0 + ar0);
            _adjustFlowRemainder(b, from, FlowRate.wrap(0), dr1, ar0, dr0);
        }
    }

    /**
     * @dev Helper function for adjusting flow remainder for the new receiver
     *
     * @param ird - Input rate difference
     * @param dr1 - ??
     */
    function _adjustFlowRemainder(Side storage a, address newRemFlowReceiver,
                                  FlowRate ird, FlowRate dr1,
                                  FlowRate ar0, FlowRate dr0) internal {
        if (a.remFlowReceiver != newRemFlowReceiver) {
            FlowRate br0 = a.token.getFlowRate(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID);
            // solve ar1 in: ird = (dr1 - dr0) + (ar1 - ar0) + (0 - br0)
            // =>
            FlowRate ar1 = ird + dr0 - dr1 + ar0 + br0;

            a.token.flow(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID, FlowRate.wrap(0));
            a.token.flow(address(this), newRemFlowReceiver, ADJUSTMENT_FLOW_ID, ar1);
            a.remFlowReceiver = newRemFlowReceiver;
        } else {
            // solve ar1 in: ird = (dr1 - dr0) + (ar1 - ar0)
            // =>
            FlowRate ar1 = ird + dr0 - dr1 + ar0;

            a.token.flow(address(this), newRemFlowReceiver, ADJUSTMENT_FLOW_ID, ar1);
        }
    }
}
