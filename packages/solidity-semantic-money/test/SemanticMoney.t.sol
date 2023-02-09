// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import "forge-std/Test.sol";
import "../src/SemanticMoney.sol";

contract SemanticMoneyTest is Test {
    using MonetaryTypes for Time;
    using MonetaryTypes for Value;
    using SemanticMoney for BasicParticle;

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

    function testSettle1Twice(BasicParticle memory p, uint16 m) external {
        assumeSafeParticle(p);
        Time t1 = p.settled_at.add(Time.wrap(m));
        BasicParticle memory p1 = p.settle1(t1);
        BasicParticle memory p2 = p1.settle1(t1);
        assertEq(Time.unwrap(p1.settled_at), Time.unwrap(t1));
        assert(eqBasicParticle(p1, p2));
    }

    function testConstantRTB(BasicParticle memory p, uint16 m1, uint16 m2) external {
        assumeSafeParticle(p);
        Time t1 = p.settled_at.add(Time.wrap(m1));
        Time t2 = t1.add(Time.wrap(m2));
        assertEq(Value.unwrap(p.settle1(t1).rtb(t2)), Value.unwrap(p.settle1(t2).rtb(t2)));
    }


    function testFlow2Flow2(uint16 m1, int64 r1, uint16 m2, int64 r2, uint16 m3) external {
        BasicParticle memory a;
        BasicParticle memory b;
        Time t1 = Time.wrap(m1);
        Time t2 = t1.add(Time.wrap(m2));
        Time t3 = t2.add(Time.wrap(m3));
        (a, b) = a.uu_flow2(b, FlowRate.wrap(r1), t1);
        (a, b) = a.uu_flow2(b, FlowRate.wrap(r2), t2);
        assertEq(0, Value.unwrap(a.rtb(t3).add(b.rtb(t3))));
    }
}
