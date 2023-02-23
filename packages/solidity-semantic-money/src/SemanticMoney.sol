// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.18;


type Time     is uint32;
type Value    is int256;
type FlowRate is int128;
type Unit     is uint128;

library MonetaryTypes {
    using MonetaryTypes for Time;
    using MonetaryTypes for Value;
    using MonetaryTypes for FlowRate;
    using MonetaryTypes for Unit;

    function add(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) + Time.unwrap(b));
    }
    function sub(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) - Time.unwrap(b));
    }

    ////////////////////////////////////////////////////////////
    // Value
    ////////////////////////////////////////////////////////////
    function inv(Value x) internal pure returns (Value) {
        return Value.wrap(-Value.unwrap(x));
    }
    function add(Value a, Value b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) + Value.unwrap(b));
    }
    function sub(Value a, Value b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) - Value.unwrap(b));
    }
    function mul(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) * int256(uint256(Unit.unwrap(b))));
    }
    function div(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) / int256(uint256(Unit.unwrap(b))));
    }

    ////////////////////////////////////////////////////////////
    // FlowRate
    ////////////////////////////////////////////////////////////
    function add(FlowRate a, FlowRate b) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(a) + FlowRate.unwrap(b));
    }
    function sub(FlowRate a, FlowRate b) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(a) - FlowRate.unwrap(b));
    }
    function inv(FlowRate r) internal pure returns (FlowRate) {
        return FlowRate.wrap(-FlowRate.unwrap(r));
    }
    function mul(FlowRate r, Time t) internal pure returns (Value) {
        return Value.wrap(FlowRate.unwrap(r) * int(uint(Time.unwrap(t))));
    }
    function mul(FlowRate r, Unit u) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(r) * int128(Unit.unwrap(u)));
    }
    function div(FlowRate a, Unit b) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(a) / int128(uint128(Unit.unwrap(b))));
    }
    function quotRem(FlowRate r, Unit u) internal pure returns (FlowRate nr, FlowRate er) {
        nr = FlowRate.wrap(FlowRate.unwrap(r) / int128(Unit.unwrap(u)));
        er = FlowRate.wrap(FlowRate.unwrap(r) % int128(Unit.unwrap(u)));
    }

    ////////////////////////////////////////////////////////////
    // Unit
    ////////////////////////////////////////////////////////////

    function add(Unit a, Unit b) internal pure returns (Unit) {
        return Unit.wrap(Unit.unwrap(a) + Unit.unwrap(b));
    }
    function sub(Unit a, Unit b) internal pure returns (Unit) {
        return Unit.wrap(Unit.unwrap(a) - Unit.unwrap(b));
    }
    function mul_u_qr_u(FlowRate r, Unit u1, Unit u2) internal pure returns (FlowRate nr, FlowRate er) {
        return r.mul(u1).quotRem(u2);
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
    using SemanticMoney for PDPoolMember;
    using SemanticMoney for PDPoolMemberMU;

    //
    // Basic Particle Operations (Also Universal Index 1-primitives)
    //

    function clone(BasicParticle memory a) internal pure returns (BasicParticle memory b) {
        // TODO memcpy
        b.settled_at = a.settled_at;
        b.settled_value = a.settled_value;
        b.flow_rate = a.flow_rate;
    }

    // monoid append
    function mappend(BasicParticle memory a, BasicParticle memory b)
        internal pure returns (BasicParticle memory c) {
        Time t = Time.unwrap(a.settled_at) > Time.unwrap(b.settled_at) ? a.settled_at : b.settled_at;
        BasicParticle memory a1 = a.settle(t);
        BasicParticle memory b1 = b.settle(t);
        c.settled_at = t;
        c.settled_value = a1.settled_value.add(b1.settled_value);
        c.flow_rate = a1.flow_rate.add(b1.flow_rate);
    }

    function settle(BasicParticle memory a, Time t) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.settled_value = rtb(a, t);
        b.settled_at = t;
    }

    function rtb(BasicParticle memory a, Time t) internal pure returns (Value v) {
        return a.flow_rate.mul(t.sub(a.settled_at)).add(a.settled_value);
    }

    function shift1(BasicParticle memory a, Value x) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.settled_value = b.settled_value.add(x); // TODO b.add_to(x);
    }

    function flow1(BasicParticle memory a, FlowRate r) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.flow_rate = r;
    }

    //
    // Universal Index to Universal Index 2-primitives
    //

    function shift2(BasicParticle memory a, BasicParticle memory b, Value x) internal pure
        returns (BasicParticle memory m, BasicParticle memory n) {
        m = a.shift1(x.inv());
        n = b.shift1(x);
    }

    function flow2(BasicParticle memory a, BasicParticle memory b, FlowRate r, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n) {
        m = a.settle(t).flow1(r.inv());
        n = b.settle(t).flow1(r);
    }

    //
    // Pool operations
    //
    // updp_: universal index to proportional distribution pool
    //

    function clone(PDPoolIndex memory a) internal pure returns (PDPoolIndex memory b) {
        b.total_units = a.total_units;
        b.wrapped_particle = a.wrapped_particle.clone();
    }

    function clone(PDPoolMember memory a) internal pure returns (PDPoolMember memory b) {
        b.owned_unit = a.owned_unit;
        b.settled_value = a.settled_value;
        b.synced_particle = a.synced_particle.clone();
    }

    function settle(PDPoolIndex memory a, Time t) internal pure
        returns (PDPoolIndex memory m)
    {
        m = a.clone();
        m.wrapped_particle = m.wrapped_particle.settle(t);
    }

    function settle(PDPoolMemberMU memory a, Time t) internal pure
        returns (PDPoolMemberMU memory b)
    {
        // TODO b.i doesn't actually change, some optimization may be desired
        b.i = a.i.clone();
        b.m = a.m.clone();
        b.m.settled_value = a.i.wrapped_particle.rtb(t)
            .sub(a.m.synced_particle.rtb(t))
            .mul(a.m.owned_unit);
    }

    function rtb(PDPoolMemberMU memory a, Time t) internal pure
        returns (Value v)
    {
        return a.i.wrapped_particle.rtb(t)
            .sub(a.m.synced_particle.rtb(a.m.synced_particle.settled_at))
            .mul(a.m.owned_unit)
            .add(a.m.settled_value);
    }

    // Update the unit amount of the member of the pool
    function pool_member_update(PDPoolMemberMU memory b1, BasicParticle memory a, Unit u, Time t)
        internal pure
        returns (PDPoolIndex memory p, PDPoolMember memory p1, BasicParticle memory b)
    {
        Unit oldTotalUnit = b1.i.total_units;
        Unit newTotalUnit = oldTotalUnit.add(u).sub(b1.m.owned_unit);
        PDPoolMemberMU memory b1s = PDPoolMemberMU(b1.i.settle(t), b1.m).settle(t);

        // align "a" because of the change of total units of the pool
        FlowRate nr = b1s.i.wrapped_particle.flow_rate;
        FlowRate er;
        if (Unit.unwrap(newTotalUnit) != 0) {
            (nr, er) = nr.mul_u_qr_u(oldTotalUnit, newTotalUnit);
            er = er;
        } else {
            er = nr.mul(oldTotalUnit);
            nr = FlowRate.wrap(0);
        }
        b1s.i.wrapped_particle = b1s.i.wrapped_particle.flow1(nr);
        b1s.i.total_units = newTotalUnit;
        b = a.settle(t).flow1(a.flow_rate.add(er));

        p = b1s.i;
        p1 = b1s.m;
        p1.owned_unit = u;
        p1.synced_particle = b1s.i.wrapped_particle.clone();
    }

    function shift2(BasicParticle memory a, PDPoolIndex memory b, Value x) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n, Value x1) {
        if (Unit.unwrap(b.total_units) != 0) {
            x1 = x.div(b.total_units).mul(b.total_units);
            m = a.shift1(x1.inv());
            n = b.clone();
            n.wrapped_particle = b.wrapped_particle.shift1(x1.div(b.total_units));
        } else {
            m = a.clone();
            n = b.clone();
        }
    }

    function flow2(BasicParticle memory a, PDPoolIndex memory b, FlowRate r, Time t) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n, FlowRate r1)
    {
        if (Unit.unwrap(b.total_units) != 0) {
            r1 = r.div(b.total_units).mul(b.total_units);
            m = a.settle(t).flow1(r1.inv());
            n = b.settle(t);
            n.wrapped_particle = n.wrapped_particle.flow1(r1.div(b.total_units));
        } else {
            m = a.settle(t).flow1(FlowRate.wrap(0));
            n = b.settle(t);
            n.wrapped_particle = n.wrapped_particle.flow1(FlowRate.wrap(0));
        }
    }

}
