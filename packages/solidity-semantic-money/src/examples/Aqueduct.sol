// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperToken.sol";


/**
 * @dev Aqueduct - a zero-intermediate liquidity market maker (ZILMM)
 */
library AqueductLibrary {
    Unit constant INIT_UNITS = Unit.wrap(1e9);

    struct SideState {
        FlowRate totalInFlowRate;
    }

    function clone(SideState memory a) internal pure returns (SideState memory b) {
        b.totalInFlowRate = a.totalInFlowRate;
    }

    function updateSide(SideState memory a,
                        Unit totalUnits, Unit curUnitsB,
                        FlowRate r0, FlowRate r1) internal pure
        returns (SideState memory m, FlowRate newDistFlowRateA, Unit newUnitsB)
    {
        m = clone(a);
        m.totalInFlowRate = a.totalInFlowRate + r1 - r0;

        // TODO: assuming 100% distributed for now
        newDistFlowRateA = m.totalInFlowRate;

        Unit newTotalUnits = FlowRate.unwrap(a.totalInFlowRate) == 0 ? INIT_UNITS :
            Unit.wrap(int128(
                          int256(FlowRate.unwrap(m.totalInFlowRate))
                          * int256(uint256(uint128(Unit.unwrap(totalUnits))))
                          / int256(FlowRate.unwrap(a.totalInFlowRate))));
        newUnitsB = curUnitsB + newTotalUnits - totalUnits;
    }
}

contract Aqueduct {
    using AqueductLibrary for AqueductLibrary.SideState;

    FlowId constant SWAP_DISTRIBUTE_FLOW_ID = FlowId.wrap(0);
    FlowId constant ADJUSTMENT_FLOW_ID = FlowId.wrap(0);

    struct Side {
        ToySuperToken token;
        ToySuperTokenPool pool;
        AqueductLibrary.SideState state;
        address remFlowReceiver;
    }

    Side internal _left;
    Side internal _right;

    function token1() external view returns (ToySuperToken) { return _left.token; }
    function token2() external view returns (ToySuperToken) { return _right.token; }
    function pool1() external view returns (ToySuperTokenPool) { return _left.pool; }
    function pool2() external view returns (ToySuperTokenPool) { return _right.pool; }

    constructor (ToySuperToken tokenL, ToySuperToken tokenR) {
        _left.token = tokenL;
        _left.pool = tokenL.createPool();
        _right.token = tokenR;
        _right.pool = tokenR.createPool();
    }

    function onFlowUpdate(ToySuperToken token, address from, FlowRate r0, FlowRate r1) external {
        if (token == _left.token) {
            _onFlowUpdate(_left, _right, from, r0, r1);
        } else if (token == _right.token) {
            _onFlowUpdate(_right, _left, from, r0, r1);
        } else revert("Unknown token");
    }

    function _onFlowUpdate(Side storage a, Side storage b, address from, FlowRate r0, FlowRate r1) internal {
        (AqueductLibrary.SideState memory sa1, FlowRate drr, Unit u1) = a.state.updateSide(
            b.pool.getTotalUnits(), b.pool.getUnits(from), r0, r1);
        a.state = sa1;

        {
            FlowRate dr0 = a.pool.getDistributionFlowRate();
            (,FlowRate dr1) = a.token.distributeFlow(address(this), a.pool, SWAP_DISTRIBUTE_FLOW_ID,
                                                     drr);
            _adjustFlowRemainder(a, from, dr0, dr1, r1 - r0);
        }

        {
            FlowRate dr0 = b.pool.getDistributionFlowRate();
            b.pool.updateMember(from, u1);
            FlowRate dr1 = b.pool.getDistributionFlowRate();

            _adjustFlowRemainder(b, from, dr0, dr1, FlowRate.wrap(0));
        }
    }

    function _adjustFlowRemainder(Side storage a, address from, FlowRate dr0, FlowRate dr1, FlowRate r) internal {
        FlowRate ar0 = a.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
        if (a.remFlowReceiver != from) {
            FlowRate br0 = a.token.getFlowRate(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID);
            // solve ar1 in: r = (dr1 - dr0) + (ar1 - ar0) + (0 - br0)
            // =>
            FlowRate ar1 = r + dr0 - dr1 + ar0 + br0;
            a.token.flow(address(this), a.remFlowReceiver, ADJUSTMENT_FLOW_ID, FlowRate.wrap(0));
            a.token.flow(address(this), from, ADJUSTMENT_FLOW_ID, ar1);
            a.remFlowReceiver = from;
        } else {
            // solve ar1 in: r = (dr1 - dr0) + (ar1 - ar0)
            // =>
            FlowRate ar1 = r + dr0 - dr1 + ar0;
            a.token.flow(address(this), from, ADJUSTMENT_FLOW_ID, ar1);
        }
    }
}
