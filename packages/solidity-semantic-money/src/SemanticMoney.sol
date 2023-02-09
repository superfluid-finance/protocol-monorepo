// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;


type Time     is uint32;
type Value    is int256;
type FlowRate is int96;

library MonetaryTypes {
    function add(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) + Time.unwrap(b));
    }
    function sub(Time a, Time b) internal pure returns (Time) {
        return Time.wrap(Time.unwrap(a) - Time.unwrap(b));
    }
    function add(Value a, Value b) internal pure returns (Value) {
        return Value.wrap(Value.unwrap(a) + Value.unwrap(b));
    }
    function inv(FlowRate r) internal pure returns (FlowRate) {
        return FlowRate.wrap(-FlowRate.unwrap(r));
    }
    function mul(FlowRate r, Time t) internal pure returns (Value) {
        return Value.wrap(FlowRate.unwrap(r) * int(uint(Time.unwrap(t))));
    }
}

struct BasicParticle {
    Time     settled_at;
    Value    settled_value;
    FlowRate flow_rate;
}

library SemanticMoney {
    using MonetaryTypes for Time;
    using MonetaryTypes for Value;
    using MonetaryTypes for FlowRate;
    using SemanticMoney for BasicParticle;

    function settle1(BasicParticle memory a, Time t) internal pure returns (BasicParticle memory b) {
        b = a;
        b.settled_value = rtb(a, t);
        b.settled_at = t;
    }

    //
    // Universal Index 1-primitives
    //

    function setFlow1(BasicParticle memory a, FlowRate r) internal pure returns (BasicParticle memory b) {
        b = a;
        b.flow_rate = r;
    }

    function rtb(BasicParticle memory a, Time t) internal pure returns (Value v) {
        return a.flow_rate.mul(t.sub(a.settled_at)).add(a.settled_value);
    }

    function uu_flow2(BasicParticle memory a, BasicParticle memory b, FlowRate r, Time t)
        internal pure returns (BasicParticle memory m, BasicParticle memory n) {
        m = a.settle1(t).setFlow1(r);
        n = b.settle1(t).setFlow1(r.inv());
    }
}
