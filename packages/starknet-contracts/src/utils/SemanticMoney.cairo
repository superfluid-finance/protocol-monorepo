%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math_cmp import is_le
from starkware.cairo.common.bool import TRUE

struct BasicParticle {
    rtb_settled_at: felt,
    rtb_settled_value: felt,
    rtb_flow_rate: felt,
}

struct PDPoolIndex {
    total_units: felt,
    wrapped_particle: BasicParticle,  // This value is measured per unit
}

struct PDPoolMember {
    owned_unit: felt,
    settled_value: felt,
    synced_particle: BasicParticle,  // It is a copy of the wrapped_particle of the index at the time an operation is performed.
}

// Pool Member Monetary unit
struct PDPoolMemberMU {
    pdPoolIndex: PDPoolIndex,
    pdPoolMember: PDPoolMember,
}

// Pure Functions

namespace SemanticMoney {
    // monoid append
    func mappend{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle
    ) -> (c: BasicParticle) {
        alloc_locals;
        let a_is_less_than_or_equal_to_b = is_le(a.rtb_settled_at, b.rtb_settled_at + 1);
        if (a_is_less_than_or_equal_to_b == TRUE) {
            let (settledBasicParticleForA) = settle(a, b.rtb_settled_at);
            let (settledBasicParticleForB) = settle(b, b.rtb_settled_at);
            let c = BasicParticle(
                b.rtb_settled_at,
                settledBasicParticleForA.rtb_settled_value +
                settledBasicParticleForB.rtb_settled_value,
                a.rtb_flow_rate + b.rtb_flow_rate,
            );
            return (c=c);
        } else {
            let (settledBasicParticleForA) = settle(a, a.rtb_settled_at);
            let (settledBasicParticleForB) = settle(b, a.rtb_settled_at);
            let c = BasicParticle(
                a.rtb_settled_at,
                settledBasicParticleForA.rtb_settled_value +
                settledBasicParticleForB.rtb_settled_value,
                a.rtb_flow_rate + b.rtb_flow_rate,
            );
            return (c=c);
        }
    }

    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (balance: felt) {
        let timeDelta = time - index.rtb_settled_at;
        let balance = (timeDelta * index.rtb_flow_rate) + index.rtb_settled_value;
        return (balance=balance);
    }

    func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (index: BasicParticle) {
        let timeDelta = time - index.rtb_settled_at;
        let newSettledValue = (timeDelta * index.rtb_flow_rate) + index.rtb_settled_value;
        let newBasicParticle = BasicParticle(time, newSettledValue, index.rtb_flow_rate);
        return (index=newBasicParticle);
    }

    func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        amount: felt, index: BasicParticle
    ) -> (index: BasicParticle, amount: felt) {
        let newBasicParticle = BasicParticle(
            index.rtb_settled_at, index.rtb_settled_value + amount, index.rtb_flow_rate
        );
        return (index=newBasicParticle, amount=amount);
    }

    func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, amount: felt, time: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (settledBasicParticleForA) = settle(a, time);
        let (settledBasicParticleForB) = settle(b, time);
        // This right-biased. That is: the `amount` argument of shift1 for a is dependent on the `amount` return by shift1 for b (RHS)
        let (bBasicParticle, _amount) = shift1(amount, settledBasicParticleForB);
        let (aBasicParticle, _) = shift1(-_amount, settledBasicParticleForA);
        return (a=aBasicParticle, b=bBasicParticle);
    }

    func setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        flow_rate: felt, index: BasicParticle
    ) -> (index: BasicParticle, flow_rate: felt) {
        let newBasicParticle = BasicParticle(
            index.rtb_settled_at, index.rtb_settled_value, flow_rate
        );
        return (index=newBasicParticle, flow_rate=flow_rate);
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, flow_rate: felt, time: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (settledBasicParticleForA) = settle(a, time);
        let (settledBasicParticleForB) = settle(b, time);
        // This is right-biased. That is: the `flow_rate` argument of setFlow1 for a is dependent on the `flow_rate` returned by  setFlow1 for b (RHS)
        let (bBasicParticle, _flow_rate) = setFlow1(flow_rate, settledBasicParticleForB);
        let (aBasicParticle, _) = setFlow1(-_flow_rate, settledBasicParticleForA);
        return (a=aBasicParticle, b=bBasicParticle);
    }

    // Pool Operations

    func realtime_balance_of_pool_member{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(poolMemberMU: PDPoolMemberMU, time: felt) -> (balance: felt) {
        let (realtime_balance_of_wrapped_particle) = realtime_balance_of(
            poolMemberMU.pdPoolIndex.wrapped_particle, time
        );
        let (realtime_balance_of_synced_particle) = realtime_balance_of(
            poolMemberMU.pdPoolMember.synced_particle,
            poolMemberMU.pdPoolMember.synced_particle.rtb_settled_at,
        );
        let balance = (realtime_balance_of_wrapped_particle - realtime_balance_of_synced_particle) *
            poolMemberMU.pdPoolMember.owned_unit;
        return (balance=balance);
    }

    func settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex, time: felt
    ) -> (index: PDPoolIndex) {
        let (settled_wrapped_particle) = settle(index.wrapped_particle, time);
        let newPoolIndex = PDPoolIndex(index.total_units, settled_wrapped_particle);
        return (index=newPoolIndex);
    }

    func settle_for_pool_member_mu{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        poolMemberMU: PDPoolMemberMU, time: felt
    ) -> (poolMemberMU: PDPoolMemberMU) {
        let (realtime_balance_of_wrapped_particle) = realtime_balance_of(
            poolMemberMU.pdPoolIndex.wrapped_particle, time
        );
        let (realtime_balance_of_synced_particle) = realtime_balance_of(
            poolMemberMU.pdPoolMember.synced_particle, time
        );
        let new_settled_value = (
            realtime_balance_of_wrapped_particle - realtime_balance_of_synced_particle
        ) * poolMemberMU.pdPoolMember.owned_unit;
        let newPoolMemberMU = PDPoolMemberMU(
            poolMemberMU.pdPoolIndex,
            PDPoolMember(
                poolMemberMU.pdPoolMember.owned_unit,
                new_settled_value,
                poolMemberMU.pdPoolMember.synced_particle,
            ),
        );
        return (poolMemberMU=newPoolMemberMU);
    }
}
