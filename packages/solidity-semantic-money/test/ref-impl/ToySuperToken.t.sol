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

    constructor () {
        N_TESTERS = TEST_ACCOUNTS.length;
    }

    function setUp() public {
        token = new ToySuperToken();
        for (uint i = 1; i < N_TESTERS; ++i) {
            vm.startPrank(admin);
            token.transfer(TEST_ACCOUNTS[i], type(uint64).max);
            vm.stopPrank();
        }
    }

    function _createPool(address by) internal returns (ToySuperTokenPool pl) {
        vm.startPrank(by);
        pl = token.createPool();
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

    function test_erc20_transfer(uint32 x) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);

        vm.startPrank(alice);
        token.transfer(bob, x);
        vm.stopPrank();

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        assertEq(a1 - a2, x);
        assertEq(b2 - b1, x);
    }

    function test_1to2_flow(uint32 r1, uint32 r2, uint16 t2) external {
        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 t1 = block.timestamp;

        vm.startPrank(alice);
        token.flow(alice, bob, FlowId.wrap(0), rr1);
        token.flow(alice, carol, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(t1 + uint256(t2));
        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);

        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), rr1, "e1.1");
        assertEq(token.getFlowRate(alice, carol, FlowId.wrap(0)), rr2, "e1.2");
        assertEq(token.getFlowRate(bob, carol, FlowId.wrap(0)), FlowRate.wrap(0), "e1.3");
        assertEq(token.getNetFlowRate(alice).inv(),
                 token.getNetFlowRate(bob) + token.getNetFlowRate(carol), "e2");
        assertEq(a1 - a2, (uint256(r1) + uint256(r2)) * uint256(t2), "e3.1");
        assertEq(a1 - a2, b2 - b1 + c2 - c1, "e3.2");
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

        vm.startPrank(bob);
        token.flow(bob, carol, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(t1 + uint256(t2));

        uint256 a2 = token.balanceOf(alice);
        uint256 b2 = token.balanceOf(bob);
        uint256 c2 = token.balanceOf(carol);

        assertEq(token.getFlowRate(alice, carol, FlowId.wrap(0)), rr1, "e1.1");
        assertEq(token.getFlowRate(bob, carol, FlowId.wrap(0)), rr2, "e1.2");
        assertEq(token.getFlowRate(alice, bob, FlowId.wrap(0)), FlowRate.wrap(0), "e1.3");
        assertEq(token.getNetFlowRate(alice) + token.getNetFlowRate(bob),
                 token.getNetFlowRate(carol).inv(), "e2");
        assertEq(a1 - a2, uint256(r1) * uint256(t2), "e3.1");
        assertEq(b1 - b2, uint256(r2) * uint256(t2), "e3.2");
        assertEq(c2 - c1, a1 - a2 + b1 - b2, "e3.3");
    }

    function test_1to2_instdistribute(uint32 u1, uint32 u2, uint64 x) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        Value xx = Value.wrap(int(uint(x)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        uint256 xxx;if (tu == 0) xxx = 0; else xxx = uint(x) / tu * tu;

        ToySuperTokenPool pl = _createPool(alice);

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
                                                    uint16 dt2, uint32 r2,
                                                    uint16 dt3) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        int256 rrr1; if (tu == 0) rrr1 = 0; else rrr1 = int256(uint256(r1) / tu * tu);
        int256 rrr2; if (tu == 0) rrr2 = 0; else rrr2 = int256(uint256(r2) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp) + dt2);
        Time t3 = t2 + Time.wrap(dt3);

        ToySuperTokenPool pl = _createPool(alice);

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(bob, uu1);
        pl.updateMember(carol, uu2);
        assertEq(pl.getClaimable(bob), Value.wrap(0), "e1.1");
        assertEq(pl.getClaimable(carol), Value.wrap(0), "e1.2");
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();

        assertEq(Unit.unwrap(pl.pendingUnits()), int(tu), "e2");

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        vm.startPrank(carol);
        token.connectPool(pl);
        vm.stopPrank();

        assertEq(pl.pendingUnits(), Unit.wrap(0), "e3.1");

        {
            FlowRate ar2 = token.getNetFlowRate(alice);
            FlowRate br2 = token.getNetFlowRate(bob);
            FlowRate cr2 = token.getNetFlowRate(carol);
            FlowRate pr2 = token.getNetFlowRate(address(pl));

            assertEq(pl.getDistributionFlowRate(), FlowRate.wrap(int128(rrr1)), "e4.1");
            assertEq(ar2, FlowRate.wrap(-int128(rrr1)), "e4.2");
            assertEq(br2 + cr2, FlowRate.wrap(int128(rrr1)), "e4.3");
            assertEq(pr2, FlowRate.wrap(0), "e4.4");
            assertEq(ar2 + br2 + cr2 + pr2, FlowRate.wrap(0), "e4.5");
        }

        vm.warp(uint256(Time.unwrap(t2)));

        vm.startPrank(alice);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr2);
        vm.stopPrank();

        vm.warp(uint256(Time.unwrap(t3)));

        {
            FlowRate ar2 = token.getNetFlowRate(alice);
            FlowRate br2 = token.getNetFlowRate(bob);
            FlowRate cr2 = token.getNetFlowRate(carol);
            FlowRate pr2 = token.getNetFlowRate(address(pl));

            assertEq(pl.getDistributionFlowRate(), FlowRate.wrap(int128(rrr2)), "e5.1");
            assertEq(ar2, FlowRate.wrap(-int128(rrr2)), "e5.2");
            assertEq(br2 + cr2, FlowRate.wrap(int128(rrr2)), "e5.3");
            assertEq(pr2, FlowRate.wrap(0), "e5.4");
            assertEq(ar2 + br2 + cr2 + pr2, FlowRate.wrap(0), "e5.5");
        }
        {
            uint256 a2 = token.balanceOf(alice);
            uint256 b2 = token.balanceOf(bob);
            uint256 c2 = token.balanceOf(carol);
            uint256 p2 = token.balanceOf(address(pl));

            assertEq(a1 - a2, uint256(rrr1) * uint256(dt2) + uint256(rrr2) * uint256(dt3), "e6.1");
            assertEq(b2 - b1 + c2 - c1 + p2 - p1, a1 - a2, "e6.2");
            assertEq(pl.getClaimable(carol) + pl.getClaimable(bob), Value.wrap(int256(a1 - a2)), "e6.1");
        }
    }

    function test_1to2_distributeflow_oneconnected(uint32 u1, uint32 u2, uint32 r, uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        Unit uu2 = Unit.wrap(int128(uint128((u2))));
        FlowRate rr = FlowRate.wrap(int64(uint64(r)));
        uint256 tu = uint(uint128(Unit.unwrap(uu1 + uu2)));
        int256 rrr; if (tu == 0) rrr = 0; else rrr = int256(uint256(r) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));

        ToySuperTokenPool pl = _createPool(alice);

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

        vm.startPrank(bob);
        token.connectPool(pl);
        vm.stopPrank();

        assertEq(pl.pendingUnits(), uu2, "e2");

        vm.warp(Time.unwrap(t2));

        {
            uint256 a2 = token.balanceOf(alice);
            uint256 b2 = token.balanceOf(bob);
            uint256 c2 = token.balanceOf(carol);
            uint256 p2 = token.balanceOf(address(pl));

            assertEq(a1 - a2, uint256(rrr) * uint256(dt2), "e5.1");
            assertEq(c2 - c1, 0, "e5.2");
            assertEq(b2 - b1 + c2 - c1 + p2 - p1, a1 - a2, "e5.3");
            assertEq(pl.getClaimable(carol) + pl.getClaimable(bob), Value.wrap(int256(a1 - a2)), "e6.1");
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

    function test_2to1_distributeflow(uint32 u1, uint32 r1, uint32 r2, uint16 dt2) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        FlowRate rr2 = FlowRate.wrap(int64(uint64(r2)));
        uint256 tu = uint(u1);
        int256 rrr1;if (tu == 0) rrr1 = 0;else rrr1 = int(uint(r1) / tu * tu);
        int256 rrr2;if (tu == 0) rrr1 = 0;else rrr2 = int(uint(r2) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));

        ToySuperTokenPool pl = _createPool(alice);

        uint256 a1 = token.balanceOf(alice);
        uint256 b1 = token.balanceOf(bob);
        uint256 c1 = token.balanceOf(carol);
        uint256 p1 = token.balanceOf(address(pl));

        vm.startPrank(alice);
        pl.updateMember(carol, uu1);
        token.distributeFlow(alice, pl, FlowId.wrap(0), rr1);
        vm.stopPrank();

        vm.startPrank(bob);
        token.distributeFlow(bob, pl, FlowId.wrap(0), rr2);
        vm.stopPrank();

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

            assertEq(pl.getDistributionFlowRate(), FlowRate.wrap(int128(rrr1 + rrr2)), "e4.1");
            assertEq(pl.getPendingDistributionFlowRate(), FlowRate.wrap(0), "e4.2");
            assertEq(ar2, FlowRate.wrap(-int128(rrr1)), "e4.3");
            assertEq(br2, FlowRate.wrap(-int128(rrr2)), "e4.4");
            assertEq(pr2, FlowRate.wrap(0), "e4.5");
            assertEq(ar2 + br2 + cr2 + pr2, FlowRate.wrap(0), "e4.6");
        }

        {
            uint256 a2 = token.balanceOf(alice);
            uint256 b2 = token.balanceOf(bob);
            uint256 c2 = token.balanceOf(carol);
            uint256 p2 = token.balanceOf(address(pl));

            assertEq(a1 - a2, uint256(rrr1) * uint256(dt2), "e5.1");
            assertEq(b1 - b2, uint256(rrr2) * uint256(dt2), "e5.2");
            assertEq(c2 - c1 + p2 - p1, a1 - a2 + b1 - b2, "e5.3");
            assertEq(pl.getClaimable(carol), Value.wrap(int256(a1 - a2 + b1 - b2)), "e6.1");
        }
    }

    function test_pool_multiple_claims(uint32 u1, uint32 r1, uint16 dt2, uint16 dt3) external {
        Unit uu1 = Unit.wrap(int128(uint128((u1))));
        FlowRate rr1 = FlowRate.wrap(int64(uint64(r1)));
        uint256 tu = uint(u1);
        int256 rrr1;if (tu == 0) rrr1 = 0;else rrr1 = int(uint(r1) / tu * tu);
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));
        Time t3 = t2 + Time.wrap(uint32(dt3));

        ToySuperTokenPool pl = _createPool(alice);

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

        assertEq(pl.getClaimable(alice) + pl.getClaimable(bob), Value.wrap(int256(a1 - a2)), "e1.1");
        assertEq(b2, b1, "e1.2");

        pl.claimAll(bob);
        assertEq(Value.unwrap(pl.getClaimable(bob)), 0, "e2.1");
        pl.claimAll(bob);
        assertEq(Value.unwrap(pl.getClaimable(bob)), 0, "e2.2");

        vm.warp(Time.unwrap(t3));

        uint256 a3 = token.balanceOf(alice);
        uint256 b3 = token.balanceOf(bob);
        uint256 p3 = token.balanceOf(address(pl));

        assertEq(a2 - a3, p3, "e3.1");
        assertEq(pl.getClaimable(bob), Value.wrap(int256(a2 - a3)), "e3.2");

        assertEq(a1 - a3, uint256(rrr1) * (uint256(dt2) + uint256(dt3)), "e4.1");
        assertEq(b3 - b1 + p3 - p1, a1 - a3, "e4.2");
    }
}
