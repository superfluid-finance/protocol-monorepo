// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;


type Time     is uint32;
type Value    is int256;
type FlowRate is int96;
type Unit     is uint128;

library MonetaryTypes {
    function add(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) + Time.unwrap(b));
    }
    function sub(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) - Time.unwrap(b));
    }

    function inv(Value x) internal pure returns (Value) {
        return Value.wrap(-Value.unwrap(x));
    }
    // TODO: add_to
    function add(Value a, Value b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) + Value.unwrap(b));
    }
    function sub(Value a, Value b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) - Value.unwrap(b));
    }

    function add(FlowRate a, FlowRate b) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(a) + FlowRate.unwrap(b));
    }
    function inv(FlowRate r) internal pure returns (FlowRate) {
        return FlowRate.wrap(-FlowRate.unwrap(r));
    }
    function mul(FlowRate r, Time t) internal pure returns (Value) {
        return Value.wrap(FlowRate.unwrap(r) * int(uint(Time.unwrap(t))));
    }

    function add(Unit a, Unit b) internal pure returns (Unit) {
        return Unit.wrap(Unit.unwrap(a) + Unit.unwrap(b));
    }
    function sub(Unit a, Unit b) internal pure returns (Unit) {
        return Unit.wrap(Unit.unwrap(a) - Unit.unwrap(b));
    }

    function mul(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) * int256(uint256(Unit.unwrap(b))));
    }

    function div(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) / int256(uint256(Unit.unwrap(b))));
    }
}

/**
 * Basic particle: building block for other indexes.
 */

struct BasicParticle {
    Time     settled_at;
    Value    settled_value;
    FlowRate flow_rate;
}

/**
 * Proportional Distribution Pool Data Structures:
 *
 * Such pool has one index and many members.
 */

struct PDPoolIndex {
    Unit          total_units;
    // The value here are usually measured per unit
    BasicParticle wrapped_particle;
}

struct PDPoolMember {
    Unit          owned_unit;
    Value         settled_value;
    // It is a copy of the wrapped_particle of the index at the time an operation is performed.
    BasicParticle synced_particle;
}

// Pool Member Monetary unit
struct PDPoolMemberMU {
    PDPoolIndex  i;
    PDPoolMember m;
}

library SemanticMoney {
    using MonetaryTypes for Time;
    using MonetaryTypes for Value;
    using MonetaryTypes for FlowRate;
    using MonetaryTypes for Unit;
    using SemanticMoney for BasicParticle;
    using SemanticMoney for PDPoolIndex;
    using SemanticMoney for PDPoolMemberMU;

    //
    // Basic Particle Operations (Also Universal Index 1-primitives)
    //

    // monoid append
    function mappend(BasicParticle memory a, BasicParticle memory b)
        internal pure returns (BasicParticle memory c) {
        Time t = Time.unwrap(a.settled_at) > Time.unwrap(b.settled_at) ? a.settled_at : b.settled_at;
        BasicParticle memory a1 = a.settle(t);
        BasicParticle memory b1 = b.settle(t);
        c.settled_value = a1.settled_value.add(b1.settled_value);
        c.flow_rate = a1.flow_rate.add(b1.flow_rate);
    }

    function settle(BasicParticle memory a, Time t) internal pure returns (BasicParticle memory b) {
        b = a;
        b.settled_value = rtb(a, t);
        b.settled_at = t;
    }

    function rtb(BasicParticle memory a, Time t) internal pure returns (Value v) {
        return a.flow_rate.mul(t.sub(a.settled_at)).add(a.settled_value);
    }

    function shift1(BasicParticle memory a, Value x) internal pure returns (BasicParticle memory b) {
        b = a;
        b.settled_value = b.settled_value.add(x); // TODO b.add_to(x);
    }

    function setFlow1(BasicParticle memory a, FlowRate r) internal pure returns (BasicParticle memory b) {
        b = a;
        b.flow_rate = r;
    }

    //
    // Universal Index to Universal Index 2-primitives
    //

    function shift2(BasicParticle memory a, BasicParticle memory b, Value x, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n) {
        m = a.settle(t).shift1(x.inv());
        n = b.settle(t).shift1(x);
    }

    function flow2(BasicParticle memory a, BasicParticle memory b, FlowRate r, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n) {
        m = a.settle(t).setFlow1(r.inv());
        n = b.settle(t).setFlow1(r);
    }

    //
    // Pool operations
    //
    // updp_: universal index to proportional distribution pool
    //

    function settle(PDPoolIndex memory a, Time t) internal pure
        returns (PDPoolIndex memory m)
    {
        m = a;
        m.wrapped_particle = m.wrapped_particle.settle(t);
    }

    function settle(PDPoolMemberMU memory a, Time t) internal pure
        returns (PDPoolMemberMU memory b)
    {
        // TODO b.i doesn't actually change, some optimization may be desired
        b = a;
        b.m.settled_value = a.i.wrapped_particle.rtb(t)
            .sub(a.m.synced_particle.rtb(t))
            .mul(a.m.owned_unit);
    }

    function rtb(PDPoolMemberMU memory a, Time t) internal pure
        returns (Value v)
    {
        return a.i.wrapped_particle.rtb(t)
            .sub(a.m.synced_particle.rtb(a.m.synced_particle.settled_at))
            .mul(a.m.owned_unit);
    }

    // Update the unit amount of the member of the pool
    function pool_member_update(PDPoolMemberMU memory b1, BasicParticle memory a, Unit u, Time t)
        internal pure
        returns (PDPoolIndex memory p, PDPoolMember memory p1, BasicParticle memory m)
    {
        PDPoolMemberMU memory b1s = PDPoolMemberMU(b1.i.settle(t), b1.m).settle(t);
        // FIXME align2
        p = b1s.i;
        p.total_units = b1s.i.total_units.add(u).sub(b1s.m.owned_unit);
        p1 = b1s.m;
        p1.owned_unit = u;
        p1.synced_particle = b1s.i.wrapped_particle;
    }

    function shift2(BasicParticle memory a, PDPoolIndex memory b, Value x, Time t) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n) {
        if (Unit.unwrap(b.total_units) != 0) {
            m = a.settle(t).shift1(x.inv());
            n = b;
            n.wrapped_particle = b.wrapped_particle.shift1(x.div(b.total_units));
        } else {
            m = a;
            n = b;
        }
    }

    function flow2(BasicParticle memory a, PDPoolIndex memory b, FlowRate r, Time t) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n)
    {
        revert("UPDP.flow2");
    }

}
