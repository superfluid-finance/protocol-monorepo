// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;


import "forge-std/Test.sol";
import "@superfluid-finance/solidity-semantic-money/src/examples/Aqueduct.sol";

import "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperToken.sol";


contract AqueductTest is Test {
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
    ToySuperToken internal token1;
    ToySuperToken internal token2;
    Aqueduct internal x;

    constructor () {
        N_TESTERS = TEST_ACCOUNTS.length;
    }

    function setUp() public {
        token1 = new ToySuperToken();
        token1.setLiquidationPeriod(Time.wrap(0));
        token2 = new ToySuperToken();
        token2.setLiquidationPeriod(Time.wrap(0));
        x = new Aqueduct(token1, token2);
        for (uint i = 1; i < N_TESTERS; ++i) {
            vm.startPrank(admin);
            token1.transfer(TEST_ACCOUNTS[i], 1e18);
            token2.transfer(TEST_ACCOUNTS[i], 1e18);
            vm.stopPrank();

            vm.startPrank(TEST_ACCOUNTS[i]);
            x.token1().connectPool(x.pool1());
            x.token2().connectPool(x.pool2());
            vm.stopPrank();
        }
    }

    function assertEq(Value a, Value b, string memory e) internal {
        assertEq(Value.unwrap(a), Value.unwrap(b), e);
    }

    function _flowWithCallback(ToySuperToken token, address from, FlowRate r) internal {
        vm.startPrank(from);
        FlowRate r0 = token.getFlowRate(from, address(x), FlowId.wrap(0));
        token.flow(from, address(x), FlowId.wrap(0), r);
        x.onFlowUpdate(token, from, r0, r);
        vm.stopPrank();
    }

    // let alice be LP, and bootstrap the liquidity
    // with the price of X/B at r1/r2
    function test_1lp_bootstrap(uint64 r1, uint64 r2, uint16 dt2) external {
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));

        FlowRate rr1 = FlowRate.wrap(int128(uint128(r1)));
        FlowRate rr2 = FlowRate.wrap(int128(uint128(r2)));

        _flowWithCallback(token1, alice, rr1);
        _flowWithCallback(token2, alice, rr2);
        _flowWithCallback(token1, alice, rr1);
        _flowWithCallback(token2, alice, rr2);

        vm.warp(Time.unwrap(t2));

        Value xl2 = token1.realtimeBalanceOf(address(x));
        Value xb2 = token2.realtimeBalanceOf(address(x));
        Value al2 = token1.realtimeBalanceOf(alice);
        Value ar2 = token2.realtimeBalanceOf(alice);

        emit log_named_int("xl2", Value.unwrap(xl2));
        emit log_named_int("xb2", Value.unwrap(xb2));
        emit log_named_int("al2", Value.unwrap(al2));
        emit log_named_int("ar2", Value.unwrap(ar2));
        emit log_named_int("pool1 tr", int256(FlowRate.unwrap(x.pool1().getDistributionFlowRate())));
        emit log_named_int("pool2 tr", int256(FlowRate.unwrap(x.pool2().getDistributionFlowRate())));
        emit log_named_int("pool1 u a", int256(Unit.unwrap(x.pool1().getUnits(alice))));
        emit log_named_int("pool2 u a", int256(Unit.unwrap(x.pool2().getUnits(alice))));
        emit log_named_int("token1 r a->x", int256(FlowRate.unwrap(token1.getFlowRate(alice, address(x), FlowId.wrap(0)))));
        emit log_named_int("token2 r a->x", int256(FlowRate.unwrap(token2.getFlowRate(alice, address(x), FlowId.wrap(0)))));
        emit log_named_int("token1 r x->a", int256(FlowRate.unwrap(token1.getFlowRate(address(x), alice, FlowId.wrap(0)))));
        emit log_named_int("token2 r x->a", int256(FlowRate.unwrap(token2.getFlowRate(address(x), alice, FlowId.wrap(0)))));

        assertEq(Value.unwrap(xl2), 0, "e1");
        assertEq(Value.unwrap(xb2), 0, "e2");
    }

    function test_1lp_2taker(uint64 r1, uint64 r2, uint16 dt2, uint64 r3, uint16 dt3) external {
        Time t2 = Time.wrap(uint32(block.timestamp)) + Time.wrap(uint32(dt2));
        Time t3 = t2 + Time.wrap(uint32(dt3));
        FlowRate rr1 = FlowRate.wrap(int128(uint128(r1)));
        FlowRate rr2 = FlowRate.wrap(int128(uint128(r2)));
        FlowRate rr3 = FlowRate.wrap(int128(uint128(r3)));

        Value al1 = token1.realtimeBalanceOf(alice);
        Value ar1 = token2.realtimeBalanceOf(alice);
        Value bl1 = token1.realtimeBalanceOf(bob);
        Value br1 = token2.realtimeBalanceOf(bob);

        _flowWithCallback(token1, bob, rr3);

        vm.warp(Time.unwrap(t2));

        _flowWithCallback(token1, alice, rr1);
        _flowWithCallback(token2, alice, rr2);

        vm.warp(Time.unwrap(t3));

        Value xl2 = token1.realtimeBalanceOf(address(x));
        Value xb2 = token2.realtimeBalanceOf(address(x));
        Value al2 = token1.realtimeBalanceOf(alice);
        Value ar2 = token2.realtimeBalanceOf(alice);
        Value bl2 = token1.realtimeBalanceOf(bob);
        Value br2 = token2.realtimeBalanceOf(bob);

        emit log_named_int("token1.rtb x", Value.unwrap(xl2));
        emit log_named_int("token1.rtb a", Value.unwrap(al2));
        emit log_named_int("token1.rtb b", Value.unwrap(bl2));
        emit log_named_int("token2.rtb x", Value.unwrap(xb2));
        emit log_named_int("token2.rtb a", Value.unwrap(ar2));
        emit log_named_int("token2.rtb b", Value.unwrap(br2));

        emit log_named_int("pool1 tr", int256(FlowRate.unwrap(x.pool1().getDistributionFlowRate())));
        emit log_named_int("pool1 u a", int256(Unit.unwrap(x.pool1().getUnits(alice))));
        emit log_named_int("pool1 u b", int256(Unit.unwrap(x.pool1().getUnits(bob))));
        emit log_named_int("token1 r a->x", int256(FlowRate.unwrap(token1.getFlowRate(alice, address(x), FlowId.wrap(0)))));
        emit log_named_int("token1 r b->x", int256(FlowRate.unwrap(token1.getFlowRate(bob, address(x), FlowId.wrap(0)))));
        emit log_named_int("token1 r x->a", int256(FlowRate.unwrap(token1.getFlowRate(address(x), alice, FlowId.wrap(0)))));
        emit log_named_int("token1 r x->b", int256(FlowRate.unwrap(token1.getFlowRate(address(x), bob, FlowId.wrap(0)))));

        emit log_named_int("pool2 tr", int256(FlowRate.unwrap(x.pool2().getDistributionFlowRate())));
        emit log_named_int("pool2 u a", int256(Unit.unwrap(x.pool2().getUnits(alice))));
        emit log_named_int("pool2 u b", int256(Unit.unwrap(x.pool2().getUnits(bob))));
        emit log_named_int("token2 r a->x", int256(FlowRate.unwrap(token2.getFlowRate(alice, address(x), FlowId.wrap(0)))));
        emit log_named_int("token2 r b->x", int256(FlowRate.unwrap(token2.getFlowRate(bob, address(x), FlowId.wrap(0)))));
        emit log_named_int("token2 r x->a", int256(FlowRate.unwrap(token2.getFlowRate(address(x), alice, FlowId.wrap(0)))));
        emit log_named_int("token2 r x->b", int256(FlowRate.unwrap(token2.getFlowRate(address(x), bob, FlowId.wrap(0)))));

        assertEq(Value.unwrap(xl2), 0, "e1.1");
        assertEq(Value.unwrap(xb2), 0, "e1.2");

        assertEq(al2 - al1, bl1 - bl2, "e2.1");
        assertEq(ar1 - ar2, br2 - br1, "e2.2");

        // Minimum swap condition: r2.div(r1 + r3) >= 1
        if (uint(r1) + uint(r3) > 0 && r3 > 0 && uint(r2) >= uint(r1) + uint(r3) &&
            dt2 > 0 && dt3 > 0) {
            assertTrue(Value.unwrap(bl2 - bl1) < 0, "e3.1");
            assertTrue(Value.unwrap(br2 - br1) > 0, "e3.2");
        }
    }

    struct Step {
        uint8  u; // which user
        uint8  t; // token 1 or token 2
        uint64 r; // flow rate
        uint16 dt; // time delta
    }
    function test_random_seqs(Step[] memory steps) external {
        uint noStepsLimit = vm.envOr("NO_FOUNDRY_TEST_STEPS_LIMIT", uint256(0));
        if (noStepsLimit == 0) {
            vm.assume(steps.length < 20);
        }
        for (uint i = 0; i < steps.length; ++i) {
            Step memory s = steps[i];
            uint u = 1 + s.u % 5; // a pool of 5 testers
            FlowRate r = FlowRate.wrap(int128(uint128(s.r)));
            _flowWithCallback(s.t % 2 == 0 ? token1 : token2, TEST_ACCOUNTS[u], r);
            vm.warp(block.timestamp + s.dt);
            emit log_named_uint("timestamp", block.timestamp);
            emit log_named_uint("tester", u);
            emit log_named_uint("token", s.t % 2 + 1);
            emit log_named_int("flow rate", FlowRate.unwrap(r));
        }
        assertEq(token1.realtimeBalanceOf(address(x)), Value.wrap(0), "e1.1");
        assertEq(token2.realtimeBalanceOf(address(x)), Value.wrap(0), "e1.2");
    }
}
