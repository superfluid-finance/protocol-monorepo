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

    function test_U_MonoidIdentity(BasicParticle memory p1) external {
        BasicParticle memory id;
        assertTrue(_eqBasicParticle(p1, p1.mappend(id)));
        assertTrue(_eqBasicParticle(p1, id.mappend(p1)));
    }

    function test_U_MonoidAssoc(BasicParticle memory p1, BasicParticle memory p2, BasicParticle memory p3) external {
        _assumeSafeParticle(p1);
        _assumeSafeParticle(p2);
        _assumeSafeParticle(p3);
        assertTrue(_eqBasicParticle(p1.mappend(p2).mappend(p3), p1.mappend(p2.mappend(p3))));
    }

    function test_U_SettleTwice(BasicParticle memory p, uint16 m) external {
        _assumeSafeParticle(p);

        Time t1 = p.settled_at + Time.wrap(m);
        BasicParticle memory p1 = p.settle(t1);
        BasicParticle memory p2 = p1.settle(t1);
        assertEq(Time.unwrap(p1.settled_at), Time.unwrap(t1));
        assertTrue(_eqBasicParticle(p1, p2));
    }

    function test_U_ConstantRTB(BasicParticle memory p, uint16 m1, uint16 m2) external {
        _assumeSafeParticle(p);

        Time t1 = p.settled_at + Time.wrap(m1);
        Time t2 = t1 + Time.wrap(m2);
        assertEq(Value.unwrap(p.settle(t1).rtb(t2)), Value.unwrap(p.settle(t2).rtb(t2)));
    }

    // Universal Index to Universal Index 2-primitives properties

    struct UU_Test_Data {
        BasicParticle a;
        BasicParticle b;
        Time t1;
        Time t2;
        Time t3;
        Time t4;
        Time t5;
    }
    function new_UU_Test_Data(uint16 m1, uint16 m2, uint16 m3)
        internal pure returns (UU_Test_Data memory d) {
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);
    }

    function test_UU_Shift2Shift2(uint16 m1, int64 x1, uint16 m2, int64 x2, uint16 m3) external {
        UU_Test_Data memory d = new_UU_Test_Data(m1, m2, m3);

        (d.a, d.b) = d.a.shift2(d.b, Value.wrap(x1));
        (d.a, d.b) = d.a.shift2(d.b, Value.wrap(x2));
        assertEq(Value.unwrap(d.a.rtb(d.t3) + d.b.rtb(d.t3)), 0);
    }

    function test_UU_Flow2Flow2(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        UU_Test_Data memory d = new_UU_Test_Data(m1, m2, m3);

        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r1), d.t1);
        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);
        assertEq(0, Value.unwrap(d.a.rtb(d.t3) + d.b.rtb(d.t3)));
    }

    function test_UU_Flow2Shift2(uint16 m1, int64 r1, uint16 m2, int64 x2, uint16 m3) external {
        UU_Test_Data memory d = new_UU_Test_Data(m1, m2, m3);

        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r1), d.t1);
        (d.a, d.b) = d.a.shift2(d.b, Value.wrap(x2));
        assertEq(Value.unwrap(d.a.rtb(d.t3) + d.b.rtb(d.t3)), 0);
    }

    function test_UU_Shift2Flow2(uint16 m1, int64 x1, uint16 m2, int64 r2, uint16 m3) external {
        UU_Test_Data memory d = new_UU_Test_Data(m1, m2, m3);

        (d.a, d.b) = d.a.shift2(d.b, Value.wrap(x1));
        (d.a, d.b) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);
        assertEq(Value.unwrap(d.a.rtb(d.t3) + d.b.rtb(d.t3)), 0);
    }

    // Universal Index to Proportional Distribution Pool

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
    function new_UPDP_Test_Data(uint16 m1, uint16 m2, uint16 m3, uint16 m4, uint16 m5)
        internal pure returns (UPDP_Test_Data memory d) {
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1 + Time.wrap(m2);
        d.t3 = d.t2 + Time.wrap(m3);
        d.t4 = d.t3 + Time.wrap(m4);
        d.t5 = d.t4 + Time.wrap(m5);
    }

    function test_UPDP_1Member_Shift2Shift2(uint16 m1, int64 u1, // update unit
                                            uint16 m2, int64 x2,  // distribute
                                            uint16 m3, int64 u2, // update unit
                                            uint16 m4, int64 x4,  // distribute
                                            uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b, ) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), u2);
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b, ) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5) + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)), 0);
    }

    function test_UPDP_1Member_Flow2Flow2(uint16 m1, int64 u1, // update unit
                                          uint16 m2, int64 r2,  // distribute flow
                                          uint16 m3, int64 u2, // update unit
                                          uint16 m4, int64 r4,  // distribute flow
                                          uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), u2);
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5) + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)), 0);
    }

    function test_UPDP_1Member_Shift2Flow2(uint16 m1, int64 u1, // update unit
                                           uint16 m2, int64 x2,  // distribute
                                           uint16 m3, int64 u2, // update unit
                                           uint16 m4, int64 r4,  // distribute flow
                                           uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), u2);
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5) + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)), 0);
    }

    function test_UPDP_1Member_Flow2Shift2(uint16 m1, int64 u1, // update unit
                                           uint16 m2, int64 r2,  // distribute flow
                                           uint16 m3, int64 u2, // update unit
                                           uint16 m4, int64 x4,  // distribute
                                           uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), u2);
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5) + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)), 0);
    }

    function test_UPDP_2Members_Shift2Shift2(uint16 m1, int64 u1, // update unit for member 1
                                             uint16 m2, int64 x2,  // distribute
                                             uint16 m3, int64 u2, // update unit for member 2
                                             uint16 m4, int64 x4,  // distribute
                                             uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), int256(u1) + int256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b2).rtb(d.t5)), 0);
    }

    function test_UPDP_2Members_Flow2Flow2(uint16 m1, int64 u1, // update unit for member 1
                                           uint16 m2, int64 r2,  // distribute flow
                                           uint16 m3, int64 u2, // update unit for member 2
                                           uint16 m4, int64 r4,  // distribute flow
                                           uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), int256(u1) + int256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b2).rtb(d.t5)), 0);
    }

    function test_UPDP_2Members_Shift2Flow2(uint16 m1, int64 u1, // update unit for member 1
                                            uint16 m2, int64 x2,  // distribute
                                            uint16 m3, int64 u2, // update unit for member 2
                                            uint16 m4, int64 r4,  // distribute flow
                                            uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), int256(u1) + int256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b2).rtb(d.t5)), 0);
    }

    function test_UPDP_2Members_Flow2Shift2(uint16 m1, int64 u1, // update unit for member 1
                                            uint16 m2, int64 r2, // distribute flow
                                            uint16 m3, int64 u2, // update unit for member 2
                                            uint16 m4, int64 x4, // distribute
                                            uint16 m5) external {
        UPDP_Test_Data memory d = new_UPDP_Test_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), int256(u1) + int256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b1).rtb(d.t5)
                              + PDPoolMemberMU(d.b, d.b2).rtb(d.t5)), 0);
    }

}
