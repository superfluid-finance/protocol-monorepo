// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperToken.sol";


contract ToySuperTokenTest is Test {
    address internal constant admin = address(0x420);
    address internal constant alice = address(0x421);
    address internal constant bob   = address(0x422);
    address internal constant carol = address(0x423);
    address internal constant dan   = address(0x424);
    address internal constant eve   = address(0x425);
    address internal constant frank = address(0x426);
    address internal constant grace = address(0x427);
    address internal constant heidi = address(0x428);
    address internal constant ivan  = address(0x429);
    uint internal immutable N_TESTERS;

    address[] internal TEST_ACCOUNTS = [admin,alice,bob,carol,dan,eve,frank,grace,heidi,ivan];
    ToySuperToken internal token;
    uint256 internal tLP;

    constructor () {
        N_TESTERS = TEST_ACCOUNTS.length;
    }

    function setUp() public {
        token = new ToySuperToken();
        tLP = uint256(Time.unwrap(token.LIQUIDATION_PERIOD()));
        for (uint i = 1; i < N_TESTERS; ++i) {
            vm.startPrank(admin);
            token.transfer(TEST_ACCOUNTS[i], type(uint64).max);
            vm.stopPrank();
        }
    }

    function _createPool(address by) internal returns (ToySuperfluidPool pl) {
        vm.startPrank(by);
        pl = token.createPool();
        vm.stopPrank();
    }

    function _connectPool(ToySuperfluidPool p, address by) internal {
        vm.startPrank(by);
        token.connectPool(p);
        vm.stopPrank();
    }

    function assertEq(FlowRate a, FlowRate b, string memory e) internal {
        assertEq(FlowRate.unwrap(a), FlowRate.unwrap(b), e);
    }
    function assertEq(Unit a, Unit b, string memory e) internal {
        assertEq(Unit.unwrap(a), Unit.unwrap(b), e);
    }
    function assertEq(Value a, Value b, string memory e) internal {
        assertEq(Value.unwrap(a), Value.unwrap(b), e);
    }
    function getAdjustmentFlowRate(ToySuperfluidPool pool, address expectedRecipient)
        internal returns (FlowRate)
    {
        (address recipient, FlowRate r) = token.getPoolAdjustmentFlowInfo(pool);
        assertEq(recipient, expectedRecipient, "expectedRecipient fail");
        return r;
    }

    function test_erc20_transfer(uint32 x1, uint32 x2) external {
        Value a1 = token.realtimeBalanceOf(alice);
        Value b1 = token.realtimeBalanceOf(bob);

        vm.startPrank(alice);
        token.transfer(bob, x1);
        vm.stopPrank();

        vm.startPrank(bob);
        token.transfer(alice, x2);
        vm.stopPrank();

        Value a2 = token.realtimeBalanceOf(alice);
        Value b2 = token.realtimeBalanceOf(bob);
        assertEq(Value.unwrap(a2 - a1), int256(uint256(x2)) - int256(uint256(x1)));
        assertEq(b2 - b1, a1 - a2, "e1");
    }

    function test_erc20_self_transfer(uint32 x1) external {
        Value a1 = token.realtimeBalanceOf(alice);
        vm.startPrank(alice);
        token.transfer(alice, x1);
        vm.stopPrank();
        Value a2 = token.realtimeBalanceOf(alice);
        assertEq(a1, a2, "e1");
    }

    function test_1to1_flow_update(uint32 r1, uint32 r2, uint16 dt1, uint16 dt2) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        Time t1 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt1));
        Time t2 = t1 + Time.wrap(uint32(dt2));

        vm.startPrank(alice);
        token.flow(alice, bob, FlowId.wrap(0), rr1);
        vm.stopPrank();
        assertEq(token.getNetFlowRate(alice), rr1.inv(), "e1.1");
        assertEq(token.getNetFlowRate(bob), rr1, "e1.2");
        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), rr1, "e1.3");

        vm.warp(Time.unwrap(t1));

        vm.startPrank(alice);
        token.flow(alice, bob, FlowId.wrap(0), rr2);
        vm.stopPrank();
        assertEq(token.getNetFlowRate(alice), rr2.inv(), "e2.1");
        assertEq(token.getNetFlowRate(bob), rr2, "e2.2");
        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), rr2, "e2.3");

        vm.warp(Time.unwrap(t2));

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 k2 = token.balanceOf(address(token));
        assertEq(a1 - a2, k2 + uint256(r1) * uint256(dt1) + uint256(r2) * uint256(dt2), "e3.1");
        assertEq(a1 - a2, k2 + b2 - b1, "e3.2");
        assertEq(k2, r2 * tLP, "e3.3");
    }

    function test_1to2_flow(uint32 r1, uint32 r2, uint16 dt1) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        Time t1 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt1));

        vm.startPrank(alice);
        token.flow(alice, bob, FlowId.wrap(0), rr1);
        token.flow(alice, carol, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(Time.unwrap(t1));
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);
        uint256 k2 = token.balanceOf(address(token));

        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), rr1, "e1.1");
        assertEq(token.getFlowRate(alice, carol, FlowId.wrap(0)), rr2, "e1.2");
        assertEq(token.getFlowRate(bob, carol, FlowId.wrap(0)), FlowRate.wrap(0), "e1.3");
        assertEq(token.getNetFlowRate(alice).inv(),
                 token.getNetFlowRate(bob) + token.getNetFlowRate(carol), "e2");
        assertEq(a1 - a2, k2 + (uint256(r1) + uint256(r2)) * uint256(dt1), "e3.1");
        assertEq(a1 - a2, k2 + b2 - b1 + c2 - c1, "e3.2");
        assertEq(k2, (uint256(r1) + uint256(r2)) * tLP, "e3.3");
    }

    function test_2to1_flow(uint32 r1, uint32 r2, uint16 t2) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 t1 = block.timestamp;

        vm.startPrank(alice);
        token.flow(alice, carol, FlowId.wrap(0), rr1);
        vm.stopPrank();
        uint256 k1 = token.balanceOf(address(token));

        vm.startPrank(bob);
        token.flow(bob, carol, FlowId.wrap(0), rr2);
        vm.stopPrank();
        uint256 k2 = token.balanceOf(address(token)) - k1;

        vm.warp(t1 + uint256(t2));

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);

        assertEq(token.getFlowRate(alice, carol, FlowId.wrap(0)), rr1, "e1.1");
        assertEq(token.getFlowRate(bob, carol, FlowId.wrap(0)), rr2, "e1.2");
        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), FlowRate.wrap(0), "e1.3");
        assertEq(token.getNetFlowRate(alice) + token.getNetFlowRate(bob),
                 token.getNetFlowRate(carol).inv(), "e2");
        assertEq(a1 - a2, k1 + uint256(r1) * uint256(t2), "e3.1");
        assertEq(b1 - b2, k2 + uint256(r2) * uint256(t2), "e3.2");
        assertEq(a1 - a2 + b1 - b2, k1 + k2 + c2 - c1, "e3.3");
        assertEq(k1, r1 * tLP, "e3.4");
        assertEq(k2, r2 * tLP, "e3.5");
    }

    function test_1to2_instdistribute(uint32 u1, uint32 u2, uint64 x) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        Value xx = Value.wrap(int(uint(x)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        uint256 xxx;if (tu == 0) xxx = 0; else xxx = uint(x) / tu * tu;

        ToySuperfluidPool pl = _createPool(alice);

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        pl.updateMember(carol, uu2);
        token.distribute(alice, pl, xx);
        vm.stopPrank();

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        vm.startPrank(carol);
        token.connectPool(pl);
        vm.stopPrank();

        Time t = Time.wrap(uint32(block.timestamp));

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);

        assertEq(Value.unwrap(pl.getClaimable(t, bob)), 0, "e1.1");
        assertEq(Value.unwrap(pl.getClaimable(t, carol)), 0, "e1.2");

        emit log_named_uint("a2", a2);
        emit log_named_uint("b2", b2);
        emit log_named_uint("c2", c2);

        assertEq(a1 - a2, xxx, "e2");
        assertEq(b2 - b1 + c2 - c1, a1 - a2, "e3");
    }

    function test_1to2_distributeflow_bothconnected(uint32 u1, uint32 u2, uint32 r1,
                                                    uint16 dt1, uint32 r2,
                                                    uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint128 tu = uint128(Unit.unwrap(uu1 + uu2));
        FlowRate rrr1; if (tu != 0) rrr1 = FlowRate.wrap(int128(uint128(r1) / tu * tu));
        FlowRate rrr2; if (tu != 0) rrr2 = FlowRate.wrap(int128(uint128(r2) / tu * tu));
        Time t1 = Time.wrap(uint32(block.timestamp) + dt1);
        Time t2 = t1 + Time.wrap(dt2);

        emit log_named_uint("u1", u1);
        emit log_named_uint("u1", u2);
        emit log_named_uint("r1", r1);
        emit log_named_int("rrr1", FlowRate.unwrap(rrr1));
        emit log_named_uint("r2", r2);
        emit log_named_int("rrr2", FlowRate.unwrap(rrr2));

        ToySuperfluidPool pl = _createPool(alice);

        Value a1 = token.realtimeBalanceOf(alice);
        Value b1 = token.realtimeBalanceOf(bob);
        Value c1 = token.realtimeBalanceOf(carol);
        Value p1 = token.realtimeBalanceOf(address(pl));

        FlowRate ar;
        FlowRate pdr;

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        pl.updateMember(carol, uu2);
        assertEq(pl.getClaimable(bob), Value.wrap(0), "e1.1");
        assertEq(pl.getClaimable(carol), Value.wrap(0), "e1.2");
        (,ar,pdr) = token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int128(tu), "e2");

        _connectPool(pl, bob);
        _connectPool(pl, carol);

        assertEq(pl.pendingUnits(), Unit.wrap(0), "e3.1");

        {
            FlowRate pdr1 = pl.getDistributionFlowRate();
            FlowRate ajr1 = getAdjustmentFlowRate(pl, alice);
            FlowRate anr1 = token.getNetFlowRate(alice);
            FlowRate bnr1 = token.getNetFlowRate(bob);
            FlowRate cnr1 = token.getNetFlowRate(carol);
            FlowRate pnr1 = token.getNetFlowRate(address(pl));

            emit log_named_int("pdr1", FlowRate.unwrap(pdr1));
            emit log_named_int("ajr1", FlowRate.unwrap(ajr1));
            emit log_named_int("anr1", FlowRate.unwrap(anr1));
            emit log_named_int("bnr1", FlowRate.unwrap(bnr1));
            emit log_named_int("cnr1", FlowRate.unwrap(cnr1));

            assertEq(pdr, pdr1, "e4.1");
            assertEq(anr1, ar.inv() + ajr1, "e4.2");
            assertEq(pdr, rrr1, "e4.3");
            assertEq(anr1, rrr1.inv() + ajr1, "e4.4");
            assertEq(bnr1 + cnr1, rrr1, "e4.5");
            assertEq(pnr1, ajr1.inv(), "e4.6");
            assertEq(anr1 + bnr1 + cnr1 + pnr1, FlowRate.wrap(0), "e4.7");
        }

        vm.warp(uint256(Time.unwrap(t1)));

        vm.startPrank(alice);
        (,ar,pdr) = token.distributeFlow(alice, pl, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(uint256(Time.unwrap(t2)));

        {
            FlowRate pdr2 = pl.getDistributionFlowRate();
            FlowRate ajr2 = getAdjustmentFlowRate(pl, alice);
            FlowRate anr2 = token.getNetFlowRate(alice);
            FlowRate bnr2 = token.getNetFlowRate(bob);
            FlowRate cnr2 = token.getNetFlowRate(carol);
            FlowRate pnr2 = token.getNetFlowRate(address(pl));

            emit log_named_int("pdr2", FlowRate.unwrap(pdr2));
            emit log_named_int("ajr2", FlowRate.unwrap(ajr2));
            emit log_named_int("anr2", FlowRate.unwrap(anr2));
            emit log_named_int("bnr2", FlowRate.unwrap(bnr2));
            emit log_named_int("cnr2", FlowRate.unwrap(cnr2));

            assertEq(pdr, pdr2, "e5.1");
            assertEq(ar.inv() + ajr2, anr2, "e5.2");
            assertEq(pdr, rrr2, "e5.3");
            assertEq(rrr2.inv() + ajr2, anr2, "e5.4");
            assertEq(bnr2 + cnr2, rrr2, "e5.5");
            assertEq(pnr2, ajr2.inv(), "e5.6");
            assertEq(anr2 + bnr2 + cnr2 + pnr2, FlowRate.wrap(0), "e5.7");
        }
        {
            Value a2 = token.realtimeBalanceOf(alice);
            Value b2 = token.realtimeBalanceOf(bob);
            Value c2 = token.realtimeBalanceOf(carol);
            Value p2 = token.realtimeBalanceOf(address(pl));
            Value k2 = token.realtimeBalanceOf(address(token));

            assertEq(a1 - a2, k2 + rrr1.mul(Time.wrap(uint32(dt1))) + rrr2.mul(Time.wrap(uint32(dt2))), "e6.1");
            assertEq(a1 - a2, k2 + b2 - b1 + c2 - c1 + p2 - p1, "e6.2");
            assertEq(a1 - a2 - k2, pl.getClaimable(carol) + pl.getClaimable(bob), "e6.3");
            assertEq(k2, rrr2.mul(Time.wrap(uint32(tLP))), "e6.4");
        }
    }

    function test_1to2_distributeflow_oneconnected(uint32 u1, uint32 u2, uint32 r,
                                                   uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        FlowRate rr = FlowRate.wrap(int64(uint64(r)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        int256 rrr; if (tu == 0) rrr = 0; else rrr = int256(uint256(r) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));

        ToySuperfluidPool pl = _createPool(alice);

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        pl.updateMember(carol, uu2);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr);
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int(tu), "e1");

        _connectPool(pl, bob);

        assertEq(pl.pendingUnits(), uu2, "e2");

        vm.warp(Time.unwrap(t2));

        {
            uint256 a2 = token.balanceOf(alice);
            uint256 b2 = token.balanceOf(bob);
            uint256 c2 = token.balanceOf(carol);
            uint256 p2 = token.balanceOf(address(pl));
            uint256 k2 = token.balanceOf(address(token));

            assertEq(a1 - a2, k2 + uint256(rrr) * uint256(dt2), "e5.1");
            assertEq(c2 - c1, 0, "e5.2");
            assertEq(a1 - a2, k2 + b2 - b1 + c2 - c1 + p2 - p1, "e5.3");
            assertEq(Value.wrap(int256(a1 - a2 - k2)), pl.getClaimable(carol) + pl.getClaimable(bob), "e5.4");
            assertEq(k2, uint256(rrr) * tLP, "e5.5");
        }

        {
            FlowRate ar2 = token.getNetFlowRate(alice);
            FlowRate br2 = token.getNetFlowRate(bob);
            FlowRate cr2 = token.getNetFlowRate(carol);
            FlowRate pr2 = token.getNetFlowRate(address(pl));

            assertEq(pl.getDistributionFlowRate(), FlowRate.wrap(int128(rrr)), "e4.1");
            assertEq(ar2, FlowRate.wrap(-int128(rrr)), "e4.2");
            assertEq(br2 + pl.getPendingDistributionFlowRate(), FlowRate.wrap(int128(rrr)), "e4.3");
            assertEq(pr2, pl.getPendingDistributionFlowRate(), "e4.4");
            assertEq(ar2 + br2 + cr2 + pr2, FlowRate.wrap(0), "e4.5");
        }
    }

    function test_1to2_distributeflow_unit_updates(uint32 u1, uint32 r1,
                                                   uint16 dt1, uint32 u2, uint32 r2,
                                                   uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        int256 rrr1; if (tu == 0) rrr1 = 0; else rrr1 = int256(uint256(r1) / tu * tu);
        int256 rrr2; if (tu == 0) rrr2 = 0; else rrr2 = int256(uint256(r2) / tu * tu);
        Time t1 = Time.wrap(uint32(block.timestamp) + dt1);
        Time t2 = t1 + Time.wrap(dt2);

        ToySuperfluidPool pl = _createPool(alice);
        _connectPool(pl, bob);

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();

        vm.warp(Time.unwrap(t1));

        vm.startPrank(alice);
        pl.updateMember(bob, uu2);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(Time.unwrap(t2));

        vm.startPrank(alice);
        token.distributeFlow(alice, pl, FlowId.wrap(0), FlowRate.wrap(0));
        vm.stopPrank();
    }

    struct PoolUpdateStep {
        uint8   u; // which user
        uint8   a; // action types: 0 update units, 1 distribute flow
        uint64  v; // action param
        uint16 dt; // time delta
    }
    function test_pool_random_seqs(PoolUpdateStep[] memory steps) external {
        ToySuperfluidPool pl = _createPool(alice);
        uint noStepsLimit = vm.envOr("NO_FOUNDRY_TEST_STEPS_LIMIT", uint256(0));
        if (noStepsLimit == 0) {
            vm.assume(steps.length < 20);
        }
        for (uint i = 0; i < steps.length; ++i) {
            emit log_named_uint(">>> step", i);

            PoolUpdateStep memory s = steps[i];
            uint a = s.a % 2;
            uint u = 1 + s.u % 5; // a pool of 5 testers including the pool creator

            emit log_named_uint("timestamp", block.timestamp);
            emit log_named_uint("tester", u);

            if (a == 0) {
                emit log_named_string("action", "updateMember");
                emit log_named_uint("unit", s.v);
                vm.startPrank(alice);
                pl.updateMember(TEST_ACCOUNTS[u], Unit.wrap(int128(uint128(s.v))));
                vm.stopPrank();
            } else if (a == 1) {
                emit log_named_string("action", "distributeFlow");
                emit log_named_uint("flow rate", s.v);
                vm.startPrank(TEST_ACCOUNTS[u]);
                (,FlowRate ar,FlowRate pdr) = token.distributeFlow
                    (TEST_ACCOUNTS[u], pl, FlowId.wrap(0), FlowRate.wrap(int128(uint128(s.v))));
                emit log_named_int("ar", FlowRate.unwrap(ar));
                emit log_named_int("pdr", FlowRate.unwrap(pdr));
                vm.stopPrank();
            } else assert(false);

            {
                FlowRate pdr = pl.getDistributionFlowRate();
                FlowRate ajr = getAdjustmentFlowRate(pl, alice);
                emit log_named_int("pdr'", FlowRate.unwrap(pdr));
                emit log_named_int("ajr", FlowRate.unwrap(ajr));
            }

            vm.warp(block.timestamp + s.dt);
        }
    }

    function test_2to1_distributeflow(uint32 u1, uint32 r1, uint32 r2, uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 tu = uint(u1);
        int256 rrr1;if (tu == 0) rrr1 = 0;else rrr1 = int(uint(r1) / tu * tu);
        int256 rrr2;if (tu == 0) rrr2 = 0;else rrr2 = int(uint(r2) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));

        ToySuperfluidPool pl = _createPool(alice);

        FlowRate ar;
        FlowRate br;
        FlowRate pdr;

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(carol, uu1);
        (,ar,) = token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();
        uint256 k1 = token.balanceOf(address(token));

        vm.startPrank(bob);
        (,br,pdr) = token.distributeFlow(bob, pl, FlowId.wrap(0), rr2);
        vm.stopPrank();
        uint256 k2 = token.balanceOf(address(token)) - k1;

        assertEq(Unit.unwrap(pl.pendingUnits()), int(tu), "e1");

        vm.startPrank(carol);
        token.connectPool(pl);
        vm.stopPrank();

        assertEq(pl.pendingUnits(), Unit.wrap(0), "e2");

        vm.warp(Time.unwrap(t2));

        {
            FlowRate ar2 = token.getNetFlowRate(alice);
            FlowRate br2 = token.getNetFlowRate(bob);
            FlowRate cr2 = token.getNetFlowRate(carol);
            FlowRate pr2 = token.getNetFlowRate(address(pl));
            FlowRate pdr2 = pl.getDistributionFlowRate();

            assertEq(pl.getDistributionFlowRate(), FlowRate.wrap(int128(rrr1 + rrr2)), "e4.1");
            assertEq(pl.getPendingDistributionFlowRate(), FlowRate.wrap(0), "e4.2");
            assertEq(ar2, FlowRate.wrap(-int128(rrr1)), "e4.3");
            assertEq(br2, FlowRate.wrap(-int128(rrr2)), "e4.4");
            assertEq(pr2, FlowRate.wrap(0), "e4.5");
            assertEq(ar2 + br2 + cr2 + pr2, FlowRate.wrap(0), "e4.6");
            assertEq(pdr2, pdr, "e4.7");
            assertEq(ar2.inv(), ar, "e4.8");
            assertEq(br2.inv(), br, "e4.9");
        }

        {
            uint256 a2 = token.balanceOf(alice);
            uint256 b2 = token.balanceOf(bob);
            uint256 c2 = token.balanceOf(carol);
            uint256 p2 = token.balanceOf(address(pl));

            assertEq(a1 - a2, k1 + uint256(rrr1) * uint256(dt2), "e5.1");
            assertEq(b1 - b2, k2 + uint256(rrr2) * uint256(dt2), "e5.2");
            assertEq(a1 - a2 + b1 - b2, k1 + k2 + c2 - c1 + p2 - p1, "e5.3");
            assertEq(Value.wrap(int256(a1 - a2 + b1 - b2 - k1 - k2)), pl.getClaimable(carol), "e5.4");
            assertEq(k1, uint256(rrr1) * tLP, "e5.5");
            assertEq(k2, uint256(rrr2) * tLP, "e5.6");
        }
    }

    function test_pool_multiple_claims(uint32 u1, uint32 r1, uint16 dt2, uint16 dt3) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        uint256 tu = uint(u1);
        int256 rrr1;if (tu == 0) rrr1 = 0;else rrr1 = int(uint(r1) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));
        Time t3 = t2 + Time.wrap(uint32(dt3));

        ToySuperfluidPool pl = _createPool(alice);

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();

        vm.warp(Time.unwrap(t2));

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 k2 = token.balanceOf(address(token));

        assertEq(Value.wrap(int256(a1 - a2 - k2)), pl.getClaimable(alice) + pl.getClaimable(bob), "e1.1");
        assertEq(b2, b1, "e1.2");

        pl.claimAll(bob);
        assertEq(Value.unwrap(pl.getClaimable(bob)), 0, "e2.1");
        pl.claimAll(bob);
        assertEq(Value.unwrap(pl.getClaimable(bob)), 0, "e2.2");

        vm.warp(Time.unwrap(t3));

        uint256 a3 = token.balanceOf(alice);
        uint256 b3 = token.balanceOf(bob);
        uint256 p3 = token.balanceOf(address(pl));
        assertEq(k2, token.balanceOf(address(token)));

        assertEq(a2 - a3, p3, "e3.1");
        assertEq(Value.wrap(int256(a2 - a3)), pl.getClaimable(bob), "e3.2");

        assertEq(a1 - a3, k2 + uint256(rrr1) * (uint256(dt2) + uint256(dt3)), "e4.1");
        assertEq(a1 - a3, k2 + b3 - b1 + p3 - p1, "e4.2");
    }
}
