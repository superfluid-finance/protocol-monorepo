// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "../src/SemanticMoney.sol";

contract SemanticMoneyTest is Test {
    function setUp() public {
    }

    // make the overflow errors go away for test cases
    function _assumeSafeParticle(BasicParticle memory p) internal pure {
        p.settled_at = Time.wrap(Time.unwrap(p.settled_at) >> 2);
        p.settled_value = Value.wrap(Value.unwrap(p.settled_value) >> 2);
        p.flow_rate = FlowRate.wrap(FlowRate.unwrap(p.flow_rate) >> 2);
    }

    function _eqBasicParticle(BasicParticle memory a, BasicParticle memory b) internal pure returns (bool) {
        return
            Time.unwrap(a.settled_at) == Time.unwrap(b.settled_at) &&
            Value.unwrap(a.settled_value) == Value.unwrap(b.settled_value) &&
            FlowRate.unwrap(a.flow_rate) == FlowRate.unwrap(b.flow_rate);
    }

    // Basic Particle/Universal Index Properties
    //
    function test_u_monoid_identity(BasicParticle memory p1) external {
        BasicParticle memory id;
        assertTrue(_eqBasicParticle(p1, p1.mappend(id)));
        assertTrue(_eqBasicParticle(p1, id.mappend(p1)));
    }

    function test_u_monoid_assoc(BasicParticle memory p1, BasicParticle memory p2, BasicParticle memory p3) external {
        _assumeSafeParticle(p1);
        _assumeSafeParticle(p2);
        _assumeSafeParticle(p3);
        assertTrue(_eqBasicParticle(p1.mappend(p2).mappend(p3), p1.mappend(p2.mappend(p3))));
    }

    function test_u_settle_idempotency(BasicParticle memory p, uint16 m) external {
        _assumeSafeParticle(p);

        Time t1 = p.settled_at + Time.wrap(m);
        BasicParticle memory p1 = p.settle(t1);
        BasicParticle memory p2 = p1.settle(t1);
        assertEq(Time.unwrap(p1.settled_at), Time.unwrap(t1));
        assertTrue(_eqBasicParticle(p1, p2));
    }

    function test_u_constant_rtb(BasicParticle memory p, uint16 m1, uint16 m2, uint16 m3) external {
        _assumeSafeParticle(p);

        Time t1 = p.settled_at + Time.wrap(m1);
        Time t2 = t1 + Time.wrap(m2);
        Time t3 = t2 + Time.wrap(m3);
        assertEq(Value.unwrap(p.settle(t1).rtb(t3)), Value.unwrap(p.settle(t2).rtb(t3)));
    }

    // Universal Index to Universal Index 2-primitives properties
    function uu_shift2(BasicParticle memory a, BasicParticle memory b, Time /* t */, int64 v)
        internal pure returns (BasicParticle memory, BasicParticle memory){
        return a.shift2(b, Value.wrap(v));
    }
    function uu_flow2(BasicParticle memory a, BasicParticle memory b, Time t, int64 v)
        internal pure returns (BasicParticle memory, BasicParticle memory) {
        return a.flow2(b, FlowRate.wrap(v), t);
    }
    struct UU_Test_Data {
        BasicParticle a;
        BasicParticle b;
        Time t1;
        Time t2;
        Time t3;
        Time t4;
        Time t5;
    }
    function run_uu_test(uint16 m1, int64 x1, uint16 m2, int64 x2, uint16 m3,
                         function (BasicParticle memory, BasicParticle memory, Time, int64)
                         internal pure returns (BasicParticle memory, BasicParticle memory) op1,
                         function (BasicParticle memory, BasicParticle memory, Time, int64)
                         internal pure returns (BasicParticle memory, BasicParticle memory) op2) internal {
        UU_Test_Data memory d;
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);

        (d.a, d.b) = op1(d.a, d.b, d.t1, x1);
        (d.a, d.b) = op2(d.a, d.b, d.t2, x2);
        assertEq(0, Value.unwrap(d.a.rtb(d.t3) + d.b.rtb(d.t3)));
    }
    function test_uu_ss(uint16 m1, int64 x1, uint16 m2, int64 x2, uint16 m3) external {
        run_uu_test(m1, x1, m2, x2, m3, uu_shift2, uu_shift2);
    }
    function test_uu_ff(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        run_uu_test(m1, r1, m2, r2, m3, uu_flow2, uu_flow2);
    }
    function test_uu_fs(uint16 m1, int64 r1, uint16 m2, int64 x2, uint16 m3) external {
        run_uu_test(m1, r1, m2, x2, m3, uu_flow2, uu_shift2);
    }
    function test_uu_sf(uint16 m1, int64 x1, uint16 m2, int64 r2, uint16 m3) external {
        run_uu_test(m1, x1, m2, r2, m3, uu_shift2, uu_flow2);
    }
    function test_uu_flow2(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        UU_Test_Data memory d;
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);

        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r1), d.t1);
        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        assertEq(Value.unwrap(d.b.rtb(d.t3)),
                 int256(r1) * int256(uint256(m2)) + int256(r2) * int256(uint256(m3)));
    }

    // Universal Index to Proportional Distribution Pool
    function updp_shift2(BasicParticle memory a, PDPoolIndex memory b, Time /* t */, int64 v)
        internal pure returns (BasicParticle memory a2, PDPoolIndex memory b2){
        (a2, b2,) = a.shift2(b, Value.wrap(v));
    }
    function updp_flow2(BasicParticle memory a, PDPoolIndex memory b, Time t, int64 v)
        internal pure returns (BasicParticle memory a2, PDPoolIndex memory b2) {
        (a2, b2,) = a.flow2(b, FlowRate.wrap(v), t);
    }
    struct UPDP_Test_Data {
        BasicParticle a;
        PDPoolIndex b;
        PDPoolMember b1;
        PDPoolMember b2;
        Time t1;
        Time t2;
        Time t3;
        Time t4;
        Time t5;
    }
    function run_updp_1m_test(// update unit
                              uint16 m1, int64 u1,
                              // op1
                              uint16 m2, int64 v2,
                              function (BasicParticle memory, PDPoolIndex memory, Time, int64)
                              internal pure returns (BasicParticle memory, PDPoolIndex memory) op1,
                              // update unit
                              uint16 m3, int64 u2,
                              // op 2
                              uint16 m4, int64 v4,
                              function (BasicParticle memory, PDPoolIndex memory, Time, int64)
                              internal pure returns (BasicParticle memory, PDPoolIndex memory) op2,
                              // final test time
                              uint16 m5) internal {
        UPDP_Test_Data memory d;
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);
        d.t4 = d.t3 + Time.wrap(m4);
        d.t5 = d.t4 + Time.wrap(m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_units), u1);

        (d.a, d.b) = op1(d.a, d.b, d.t2,v2);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), u2);
        assertEq(Unit.unwrap(d.b1.owned_units), u2);

        (d.a, d.b) = op2(d.a, d.b, d.t4,v4);

        assertEq(Value.unwrap(d.a.rtb(d.t5) + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)), 0);
    }
    function test_updp_1m_ss(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 x2, // distribute
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 x4, // distribute
                             uint16 m5) external {
        run_updp_1m_test(m1, u1, m2, x2, updp_shift2, m3, u2, m4, x4, updp_shift2, m5);
    }
    function test_updp_1m_ff(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 r2, // distribute flow
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 r4, // distribute flow
                             uint16 m5) external {
        run_updp_1m_test(m1, u1, m2, r2, updp_flow2, m3, u2, m4, r4, updp_flow2, m5);
    }
    function test_updp_1m_sf(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 x2, // distribute
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 r4, // distribute flow
                             uint16 m5) external {
        run_updp_1m_test(m1, u1, m2, x2, updp_shift2, m3, u2, m4, r4, updp_flow2, m5);
    }
    function test_updp_1m_fs(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 r2, // distribute flow
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 x4, // distribute
                             uint16 m5) external {
        run_updp_1m_test(m1, u1, m2, r2, updp_flow2, m3, u2, m4, x4, updp_shift2, m5);
    }

    function run_updp_2m_test(// update unit
                              uint16 m1, int64 u1,
                              // op1
                              uint16 m2, int64 v2,
                              function (BasicParticle memory, PDPoolIndex memory, Time, int64)
                              internal pure returns (BasicParticle memory, PDPoolIndex memory) op1,
                              // update unit
                              uint16 m3, int64 u2,
                              // op 2
                              uint16 m4, int64 v4,
                              function (BasicParticle memory, PDPoolIndex memory, Time, int64)
                              internal pure returns (BasicParticle memory, PDPoolIndex memory) op2,
                              // final test time
                              uint16 m5) internal {
        UPDP_Test_Data memory d;
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);
        d.t4 = d.t3 + Time.wrap(m4);
        d.t5 = d.t4 + Time.wrap(m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_units), u1);

        (d.a, d.b) = op1(d.a, d.b, d.t2, v2);

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), int256(u1) + int256(u2));
        assertEq(Unit.unwrap(d.b2.owned_units), u2);

        (d.a, d.b) = op2(d.a, d.b, d.t4, v4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b2).rtb(d.t5)), 0);
    }
    function test_updp_2m_ss(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 x2, // distribute
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 x4, // distribute
                             uint16 m5) external {
        run_updp_2m_test(m1, u1, m2, x2, updp_shift2, m3, u2, m4, x4, updp_shift2, m5);
    }
    function test_updp_2m_ff(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 r2, // distribute flow
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 r4, // distribute flow
                             uint16 m5) external {
        run_updp_2m_test(m1, u1, m2, r2, updp_flow2, m3, u2, m4, r4, updp_flow2, m5);
    }
    function test_updp_2m_sf(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 x2, // distribute
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 r4, // distribute flow
                             uint16 m5) external {
        run_updp_2m_test(m1, u1, m2, x2, updp_shift2, m3, u2, m4, r4, updp_flow2, m5);
    }
    function test_updp_2m_fs(uint16 m1, int64 u1, // update unit
                             uint16 m2, int64 r2, // distribute flow
                             uint16 m3, int64 u2, // update unit
                             uint16 m4, int64 x4, // distribute
                             uint16 m5) external {
        run_updp_2m_test(m1, u1, m2, r2, updp_flow2, m3, u2, m4, x4, updp_shift2, m5);
    }
    function test_updp_flow2(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        UPDP_Test_Data memory d;
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(1), d.t1);
        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r1), d.t1);
        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        assertEq(Value.unwrap(PDPoolMemberMU(d.b, d.b1).rtb(d.t3)),
                 int256(r1) * int256(uint256(m2)) + int256(r2) * int256(uint256(m3)));
    }
}
