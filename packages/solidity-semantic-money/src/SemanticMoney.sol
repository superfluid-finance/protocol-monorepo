// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;


/*******************************************************************************
 * On Coding Style: Functional Programming In Solidity
 *
 * This library is a translation of the Haskell Specification of Semantic Money.
 *
 * All functions are pure functions, more so than the "pure" solidity function
 * in that memory input data are always cloned. This makes true referential
 * transparency for all functions defined here.
 *
 * To visually inform the library users about this paradigm, the coding style
 * is deliberately chosen to go against the commonly recommended solhint sets.
 * Namely:
 *
 * - All library and "free range" function names are in snake_cases.
 * - All struct variables are in snake_cases.
 * - All types are in capitalized CamelCases.
 * - Comments are scarce, and written only for solidity specifics. This is to
 *   minimize regurgitation of the facts and keep original the original
 *   information where it belongs to. The clarity of the semantics and grunular
 *   of the API should compensate for that controversial take.
 */
// solhint-disable func-name-mixedcase
// solhint-disable var-name-mixedcase

////////////////////////////////////////////////////////////////////////////////
// Monetary Types and Their Helpers
////////////////////////////////////////////////////////////////////////////////

/// Time value represented by uint32 unix timestamp.
type Time is uint32;
function mt_t_add_t(Time a, Time b) pure returns (Time) { return Time.wrap(Time.unwrap(a) + Time.unwrap(b)); }
function mt_t_sub_t(Time a, Time b) pure returns (Time) { return Time.wrap(Time.unwrap(a) - Time.unwrap(b)); }
using { mt_t_add_t as +, mt_t_sub_t as - } for Time global;

/// Monetary value represented with signed integer.
type Value is int256;
function mt_v_add_v(Value a, Value b) pure returns (Value) {
    return Value.wrap(Value.unwrap(a) + Value.unwrap(b));
}
function mt_v_sub_v(Value a, Value b) pure returns (Value) {
    return Value.wrap(Value.unwrap(a) - Value.unwrap(b));
}
using { mt_v_add_v as +, mt_v_sub_v as - } for Value global;

/// Unit value represented with half the size of `Value`.
type Unit is int128;
function mt_u_add_u(Unit a, Unit b) pure returns (Unit) {
    return Unit.wrap(Unit.unwrap(a) + Unit.unwrap(b));
}
function mt_u_sub_u(Unit a, Unit b) pure returns (Unit) {
    return Unit.wrap(Unit.unwrap(a) - Unit.unwrap(b));
}
using { mt_u_add_u as +, mt_u_sub_u as - } for Unit global;

/**
 * @dev FlowRate value represented with half the size of `Value`.
 *
 * It is important to make sure that `FlowRate` multiplying with either `Time`
 * or `Unit` does not exceed the range of `Value`.
 */
type FlowRate is int128;
function mt_r_add_r(FlowRate a, FlowRate b) pure returns (FlowRate) {
    return FlowRate.wrap(FlowRate.unwrap(a) + FlowRate.unwrap(b));
}
function mt_r_sub_r(FlowRate a, FlowRate b) pure returns (FlowRate) {
    return FlowRate.wrap(FlowRate.unwrap(a) - FlowRate.unwrap(b));
}
using { mt_r_add_r as +, mt_r_sub_r as - } for FlowRate global;

/**
 * @dev Additional helper functions for the monetary types
 *
 * Note that due to solidity current limitations, operators for mixed user defined value types
 * are not supported, hence the need of this library.
 * Read more at: https://github.com/ethereum/solidity/issues/11969#issuecomment-1448445474
 */
library AdditionalMonetaryTypeHelpers {
    function inv(Value x) internal pure returns (Value) {
        return Value.wrap(-Value.unwrap(x));
    }
    function mul(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) * int256(Unit.unwrap(b)));
    }
    function div(Value a, Unit b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) / int256(Unit.unwrap(b)));
    }

    function inv(FlowRate r) internal pure returns (FlowRate) {
        return FlowRate.wrap(-FlowRate.unwrap(r));
    }

    function mul(FlowRate r, Time t) internal pure returns (Value) {
        return Value.wrap(FlowRate.unwrap(r) * int(uint(Time.unwrap(t))));
    }
    function mul(FlowRate r, Unit u) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(r) * Unit.unwrap(u));
    }
    function div(FlowRate a, Unit b) internal pure returns (FlowRate) {
        return FlowRate.wrap(FlowRate.unwrap(a) / Unit.unwrap(b));
    }
    function quotrem(FlowRate r, Unit u) internal pure returns (FlowRate nr, FlowRate er) {
        // quotient and remainder
        nr = FlowRate.wrap(FlowRate.unwrap(r) / Unit.unwrap(u));
        er = FlowRate.wrap(FlowRate.unwrap(r) % Unit.unwrap(u));
    }
    function mul_quotrem(FlowRate r, Unit u1, Unit u2) internal pure returns (FlowRate nr, FlowRate er) {
        return r.mul(u1).quotrem(u2);
    }
}
using AdditionalMonetaryTypeHelpers for Time global;
using AdditionalMonetaryTypeHelpers for Value global;
using AdditionalMonetaryTypeHelpers for FlowRate global;
using AdditionalMonetaryTypeHelpers for Unit global;

////////////////////////////////////////////////////////////////////////////////
// Basic particle
////////////////////////////////////////////////////////////////////////////////
/**
 * @dev Basic particle: the building block for payment primitives.
 */
struct BasicParticle {
    Time     settled_at;
    Value    settled_value;
    FlowRate flow_rate;
}

/// Return a monoidal empty value (identity element) for BasicParticle
// solhint-disable-next-line no-empty-blocks
function mempty_basic_particle() pure returns (BasicParticle memory) {}

////////////////////////////////////////////////////////////////////////////////
// Proportional Distribution Pool Data Structures.
//
// Such pool has one index and many members.
////////////////////////////////////////////////////////////////////////////////
/**
 * @dev Proportional distribution pool index data.
 */
struct PDPoolIndex {
    Unit          total_units;
    // The value here are usually measured per unit
    BasicParticle wrapped_particle;
}

/**
 * @dev Proportional distribution pool member data.
 */
struct PDPoolMember {
    Unit          owned_units;
    Value         settled_value;
    // It is a copy of the wrapped_particle of the index at the time an operation is performed.
    BasicParticle synced_particle;
}

/**
 * @dev Proportional distribution pool "monetary unit" for a member.
 */
struct PDPoolMemberMU {
    PDPoolIndex  i;
    PDPoolMember m;
}

/**
 * @dev Semantic Money Library: providing generalized payment primitives.
 *
 * Notes:
 *
 * - Basic payment 2-primitives include shift2 and flow2.
 * - As its name suggesting, 2-primitives work over two parties, each party is represented by an "index".
 * - A universal index is BasicParticle plus being a Monoid. It is universal in the sense that every monetary
 * unit should have one and only one such index.
 * - Proportional distribution pool has one index per pool.
 * - This solidity library provides 2-primitives for `UniversalIndex-to-UniversalIndex` and
 *   `UniversalIndex-to-ProportionalDistributionPoolIndex`.
 */
library SemanticMoney {
    //
    // Basic Particle Operations
    //

    /// Pure data clone function.
    function clone(BasicParticle memory a) internal pure returns (BasicParticle memory b) {
        // TODO memcpy
        b.settled_at = a.settled_at;
        b.settled_value = a.settled_value;
        b.flow_rate = a.flow_rate;
    }

    /// Monetary unit settle function for basic particle/universal index.
    function settle(BasicParticle memory a, Time t) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.settled_value = rtb(a, t);
        b.settled_at = t;
    }

    /// Monetary unit rtb function for basic particle/universal index.
    function rtb(BasicParticle memory a, Time t) internal pure returns (Value v) {
        return a.flow_rate.mul(t - a.settled_at) + a.settled_value;
    }

    function shift1(BasicParticle memory a, Value x) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.settled_value = b.settled_value + x;
    }

    function flow1(BasicParticle memory a, FlowRate r) internal pure returns (BasicParticle memory b) {
        b = a.clone();
        b.flow_rate = r;
    }

    //
    // Universal Index Additional Operations
    //

    // Note: the identity element is trivial, the default BasicParticle value will do.

    /// Monoid binary operator for basic particle/universal index.
    function mappend(BasicParticle memory a, BasicParticle memory b)
        internal pure returns (BasicParticle memory c)
    {
        // Note that the original spec abides the monoid laws even when time value is negative.
        Time t = Time.unwrap(a.settled_at) > Time.unwrap(b.settled_at) ? a.settled_at : b.settled_at;
        BasicParticle memory a1 = a.settle(t);
        BasicParticle memory b1 = b.settle(t);
        c.settled_at = t;
        c.settled_value = a1.settled_value + b1.settled_value;
        c.flow_rate = a1.flow_rate + b1.flow_rate;
    }

    //
    // Universal Index to Universal Index 2-primitives
    //

    function shift2(BasicParticle memory a, BasicParticle memory b, Value x) internal pure
        returns (BasicParticle memory m, BasicParticle memory n)
    {
        m = a.shift1(x.inv());
        n = b.shift1(x);
    }

    function flow2(BasicParticle memory a, BasicParticle memory b, FlowRate r, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n)
    {
        m = a.settle(t).flow1(r.inv());
        n = b.settle(t).flow1(r);
    }

    // Note: This note that all shift_flow variants have the same code "shape".
    //
    //       This should not be a surprise because they should be one polymorphic function had
    //       solidity support some sort of parametric polymorphism.
    function shift_flow2a(BasicParticle memory a, BasicParticle memory b, FlowRate dr, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n)
    {
        BasicParticle memory mempty;
        BasicParticle memory b1;
        BasicParticle memory b2;
        ( , b1) = a.flow2(mempty, a.flow_rate, t);
        (m, b2) = a.flow2(mempty, a.flow_rate.inv() + dr, t);
        n = b.mappend(b1).mappend(b2);
    }

    // Note: This is functionally identity to shift_flow2a for (BasicParticle, BasicParticle).
    //       This is a included to keep fidelity with the specification.
    function shift_flow2b(BasicParticle memory a, BasicParticle memory b, FlowRate dr, Time t) internal pure
        returns (BasicParticle memory m, BasicParticle memory n)
    {
        BasicParticle memory mempty;
        BasicParticle memory a1;
        BasicParticle memory a2;
        (a1,  ) = mempty.flow2(b, b.flow_rate.inv(), t);
        (a2, n) = mempty.flow2(b, b.flow_rate + dr, t);
        m = a.mappend(a1).mappend(a2);
    }

    //
    // Proportional Distribution Pool Index Operations
    //

    /// Pure data clone function.
    function clone(PDPoolIndex memory a) internal pure
        returns (PDPoolIndex memory b)
    {
        b.total_units = a.total_units;
        b.wrapped_particle = a.wrapped_particle.clone();
    }

    /// Monetary unit settle function for pool index.
    function settle(PDPoolIndex memory a, Time t) internal pure
        returns (PDPoolIndex memory m)
    {
        m = a.clone();
        m.wrapped_particle = m.wrapped_particle.settle(t);
    }

    function shift2(BasicParticle memory a, PDPoolIndex memory b, Value x) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n, Value x1)
    {
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

    function shift_flow2b(BasicParticle memory a, PDPoolIndex memory b, FlowRate dr, Time t) internal pure
        returns (BasicParticle memory m, PDPoolIndex memory n, FlowRate r1)
    {
        BasicParticle memory mempty;
        BasicParticle memory a1;
        BasicParticle memory a2;
        FlowRate r = b.wrapped_particle.flow_rate.mul(b.total_units);
        (a1,  ,   ) = mempty.flow2(b, r.inv(), t);
        (a2, n, r1) = mempty.flow2(b, r + dr, t);
        m = a.mappend(a1).mappend(a2);
    }

    //
    // Proportional Distribution Pool Member Operations
    //

    /// Pure data clone function.
    function clone(PDPoolMember memory a) internal pure
        returns (PDPoolMember memory b)
    {
        b.owned_units = a.owned_units;
        b.settled_value = a.settled_value;
        b.synced_particle = a.synced_particle.clone();
    }

    /// Monetary unit settle function for pool member.
    function settle(PDPoolMemberMU memory a, Time t) internal pure
        returns (PDPoolMemberMU memory b)
    {
        b.i = a.i.settle(t);
        b.m = a.m.clone();
        b.m.settled_value = a.rtb(t);
        b.m.synced_particle = b.i.wrapped_particle;
    }

    /// Monetary unit rtb function for pool member.
    function rtb(PDPoolMemberMU memory a, Time t) internal pure
        returns (Value v)
    {
        return a.m.settled_value +
            (a.i.wrapped_particle.rtb(t) - a.m.synced_particle.rtb(a.m.synced_particle.settled_at))
            .mul(a.m.owned_units);
    }

    /// Update the unit amount of the member of the pool
    function pool_member_update(PDPoolMemberMU memory b1, BasicParticle memory a, Unit u, Time t) internal pure
        returns (PDPoolIndex memory p, PDPoolMember memory p1, BasicParticle memory b)
    {
        Unit oldTotalUnit = b1.i.total_units;
        Unit newTotalUnit = oldTotalUnit + u - b1.m.owned_units;
        PDPoolMemberMU memory b1s = b1.settle(t);

        // align "a" because of the change of total units of the pool
        FlowRate nr = b1s.i.wrapped_particle.flow_rate;
        FlowRate er;
        if (Unit.unwrap(newTotalUnit) != 0) {
            (nr, er) = nr.mul_quotrem(oldTotalUnit, newTotalUnit);
            er = er;
        } else {
            er = nr.mul(oldTotalUnit);
            nr = FlowRate.wrap(0);
        }
        b1s.i.wrapped_particle = b1s.i.wrapped_particle.flow1(nr);
        b1s.i.total_units = newTotalUnit;
        b = a.settle(t).flow1(a.flow_rate + er);

        p = b1s.i;
        p1 = b1s.m;
        p1.owned_units = u;
        p1.synced_particle = b1s.i.wrapped_particle.clone();
    }
}
using SemanticMoney for BasicParticle global;
using SemanticMoney for PDPoolIndex global;
using SemanticMoney for PDPoolMember global;
using SemanticMoney for PDPoolMemberMU global;
