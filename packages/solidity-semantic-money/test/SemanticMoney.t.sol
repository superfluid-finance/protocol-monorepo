// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.18;

import "forge-std/Test.sol";
import "../src/SemanticMoney.sol";

contract SemanticMoneyTest is Test {
    using MonetaryTypes for Time;
    using MonetaryTypes for Value;
    using SemanticMoney for BasicParticle;
    using SemanticMoney for PDPoolMemberMU;

    function setUp() public {
    }

    function eqBasicParticle(BasicParticle memory a, BasicParticle memory b) internal pure returns (bool) {
        return
            Time.unwrap(a.settled_at) == Time.unwrap(b.settled_at) &&
            Value.unwrap(a.settled_value) == Value.unwrap(b.settled_value) &&
            FlowRate.unwrap(a.flow_rate) == FlowRate.unwrap(b.flow_rate);
    }

    function assumeSafeParticle(BasicParticle memory p) internal {
        vm.assume(Time.unwrap(p.settled_at) < uint256(type(uint16).max));
        vm.assume(Value.unwrap(p.settled_value) > 0);
        vm.assume(Value.unwrap(p.settled_value) < int256(uint256(type(uint128).max)));
    }

    // settle properties
    //

    function test_UU_SettleTwice(BasicParticle memory p, uint16 m) external {
        assumeSafeParticle(p);
        Time t1 = p.settled_at.add(Time.wrap(m));
        BasicParticle memory p1 = p.settle(t1);
        BasicParticle memory p2 = p1.settle(t1);
        assertEq(Time.unwrap(p1.settled_at), Time.unwrap(t1));
        assert(eqBasicParticle(p1, p2));
    }

    function test_UU_ConstantRTB(BasicParticle memory p, uint16 m1, uint16 m2) external {
        assumeSafeParticle(p);
        Time t1 = p.settled_at.add(Time.wrap(m1));
        Time t2 = t1.add(Time.wrap(m2));
        assertEq(Value.unwrap(p.settle(t1).rtb(t2)), Value.unwrap(p.settle(t2).rtb(t2)));
    }


    // Universal Index to Universal Index 2-primitives properties

    function test_UU_Shift2Shift2(uint16 m1, int64 x1, uint16 m2, int64 x2, uint16 m3) external {
        BasicParticle memory a;
        BasicParticle memory b;
        Time t1 = Time.wrap(m1);
        Time t2 = t1.add(Time.wrap(m2));
        Time t3 = t2.add(Time.wrap(m3));

        (a, b) = a.shift2(b, Value.wrap(x1));
        (a, b) = a.shift2(b, Value.wrap(x2));
        assertEq(Value.unwrap(a.rtb(t3).add(b.rtb(t3))), 0);
    }

    function test_UU_Flow2Flow2(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        BasicParticle memory a;
        BasicParticle memory b;
        Time t1 = Time.wrap(m1);
        Time t2 = t1.add(Time.wrap(m2));
        Time t3 = t2.add(Time.wrap(m3));

        (a, b) = a.flow2(b, FlowRate.wrap(r1), t1);
        (a, b) = a.flow2(b, FlowRate.wrap(r2), t2);
        assertEq(0, Value.unwrap(a.rtb(t3).add(b.rtb(t3))));
    }

    function test_UU_Flow2Shift2(uint16 m1, int64 r1, uint16 m2, int64 x2, uint16 m3) external {
        BasicParticle memory a;
        BasicParticle memory b;
        Time t1 = Time.wrap(m1);
        Time t2 = t1.add(Time.wrap(m2));
        Time t3 = t2.add(Time.wrap(m3));

        (a, b) = a.flow2(b, FlowRate.wrap(r1), t1);
        (a, b) = a.shift2(b, Value.wrap(x2));
        assertEq(Value.unwrap(a.rtb(t3).add(b.rtb(t3))), 0);
    }

    function test_UU_Shift2Flow2(uint16 m1, int64 x1, uint16 m2, int64 r2, uint16 m3) external {
        BasicParticle memory a;
        BasicParticle memory b;
        Time t1 = Time.wrap(m1);
        Time t2 = t1.add(Time.wrap(m2));
        Time t3 = t2.add(Time.wrap(m3));

        (a, b) = a.shift2(b, Value.wrap(x1));
        (a, b) = a.flow2(b, FlowRate.wrap(r2), t2);
        assertEq(Value.unwrap(a.rtb(t3).add(b.rtb(t3))), 0);
    }

    // Universal Index to Proportional Distribution Pool

    struct UPDP_2Members_Data {
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
    function new_UPDP_2Members_Data(uint16 m1, uint16 m2, uint16 m3, uint16 m4, uint16 m5)
        internal pure returns (UPDP_2Members_Data memory d) {
        d.t1 = Time.wrap(m1);
        d.t2 = d.t1.add(Time.wrap(m2));
        d.t3 = d.t2.add(Time.wrap(m3));
        d.t4 = d.t3.add(Time.wrap(m4));
        d.t5 = d.t4.add(Time.wrap(m5));

    }

    function test_UPDP_1Member_Shift2Shift2(uint16 m1, uint64 u1, // update unit
                                            uint16 m2, int64 x2,  // distribute
                                            uint16 m3, uint64 u2, // update unit
                                            uint16 m4, int64 x4,  // distribute
                                            uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b, ) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u2));
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b, ) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_1Member_Flow2Flow2(uint16 m1, uint64 u1, // update unit
                                          uint16 m2, int64 r2,  // distribute flow
                                          uint16 m3, uint64 u2, // update unit
                                          uint16 m4, int64 r4,  // distribute flow
                                          uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u2));
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_1Member_Shift2Flow2(uint16 m1, uint64 u1, // update unit
                                           uint16 m2, int64 x2,  // distribute
                                           uint16 m3, uint64 u2, // update unit
                                           uint16 m4, int64 r4,  // distribute flow
                                           uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u2));
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_1Member_Flow2Shift2(uint16 m1, uint64 u1, // update unit
                                           uint16 m2, int64 r2,  // distribute flow
                                           uint16 m3, uint64 u2, // update unit
                                           uint16 m4, int64 x4,  // distribute
                                           uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u2));
        assertEq(Unit.unwrap(d.b1.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_2Members_Shift2Shift2(uint16 m1, uint64 u1, // update unit for member 1
                                             uint16 m2, int64 x2,  // distribute
                                             uint16 m3, uint64 u2, // update unit for member 2
                                             uint16 m4, int64 x4,  // distribute
                                             uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u1) + uint256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                              .add(PDPoolMemberMU(d.b, d.b2).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_2Members_Flow2Flow2(uint16 m1, uint64 u1, // update unit for member 1
                                           uint16 m2, int64 r2,  // distribute flow
                                           uint16 m3, uint64 u2, // update unit for member 2
                                           uint16 m4, int64 r4,  // distribute flow
                                           uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u1) + uint256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                              .add(PDPoolMemberMU(d.b, d.b2).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_2Members_Shift2Flow2(uint16 m1, uint64 u1, // update unit for member 1
                                            uint16 m2, int64 x2,  // distribute
                                            uint16 m3, uint64 u2, // update unit for member 2
                                            uint16 m4, int64 r4,  // distribute flow
                                            uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x2));

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u1) + uint256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r4), d.t4);

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                              .add(PDPoolMemberMU(d.b, d.b2).rtb(d.t5))
                             ), 0);
    }

    function test_UPDP_2Members_Flow2Shift2(uint16 m1, uint64 u1, // update unit for member 1
                                            uint16 m2, int64 r2,  // distribute flow
                                            uint16 m3, uint64 u2, // update unit for member 2
                                            uint16 m4, int64 x4,  // distribute
                                            uint16 m5) external {
        UPDP_2Members_Data memory d = new_UPDP_2Members_Data(m1, m2, m3, m4, m5);

        (d.b, d.b1, d.a) = PDPoolMemberMU(d.b, d.b1).pool_member_update(d.a, Unit.wrap(u1), d.t1);
        assertEq(Value.unwrap(d.b1.synced_particle.settled_value), 0);
        assertEq(Unit.unwrap(d.b.total_units), u1);
        assertEq(Unit.unwrap(d.b1.owned_unit), u1);

        (d.a, d.b,) = d.a.flow2(d.b, FlowRate.wrap(r2), d.t2);

        (d.b, d.b2, d.a) = PDPoolMemberMU(d.b, d.b2).pool_member_update(d.a, Unit.wrap(u2), d.t3);
        assertEq(Unit.unwrap(d.b.total_units), uint256(u1) + uint256(u2));
        assertEq(Unit.unwrap(d.b2.owned_unit), u2);

        (d.a, d.b,) = d.a.shift2(d.b, Value.wrap(x4));

        assertEq(Value.unwrap(d.a.rtb(d.t5)
                              .add(PDPoolMemberMU(d.b, d.b1).rtb(d.t5))
                              .add(PDPoolMemberMU(d.b, d.b2).rtb(d.t5))
                             ), 0);
    }

}
