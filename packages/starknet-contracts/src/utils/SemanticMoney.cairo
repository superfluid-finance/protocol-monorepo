%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem
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
        let a_is_less_than_or_equal_to_b = is_le(a.rtb_settled_at, b.rtb_settled_at);
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

    func shiftFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        flow_rate: felt, index: BasicParticle
    ) -> (index: BasicParticle, flow_rate: felt) {
        let newBasicParticle = BasicParticle(
            index.rtb_settled_at, index.rtb_settled_value, index.rtb_flow_rate + flow_rate
        );
        return (index=newBasicParticle, flow_rate=flow_rate);
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, flow_rate: felt, time: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (settledBasicParticleForA) = settle(a, time);
        let (settledBasicParticleForB) = settle(b, time);
        // This is right-biased. That is: the `flow_rate` argument of shiftFlow1 for a is dependent on the `flow_rate` returned by  shiftFlow1 for b (RHS)
        let (bBasicParticle, _flow_rate) = shiftFlow1(flow_rate, settledBasicParticleForB);
        let (aBasicParticle, _) = shiftFlow1(-_flow_rate, settledBasicParticleForA);
        return (a=aBasicParticle, b=bBasicParticle);
    }

    // Pool Operations

    func clone_of_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (index: PDPoolIndex) {
        let clonedPoolIndex = PDPoolIndex(
            index.total_units,
            BasicParticle(
                index.wrapped_particle.rtb_settled_at, index.rtb_settled_value, index.rtb_flow_rate
            ),
        );
        return (index=clonedPoolIndex);
    }

    func clone_of_pool_member_mu{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        poolMemberMU: PDPoolMemberMU
    ) -> (poolMemberMU: PDPoolMemberMU) {
        let clonedPoolMemberMU = PDPoolMemberMU(
            PDPoolIndex(
                poolMemberMU.pdPoolIndex.total_units, poolMemberMU.pdPoolIndex.wrapped_particle
            ),
            PDPoolMember(
                poolMemberMU.pdPoolMember.owned_unit,
                poolMemberMU.pdPoolMember.settled_value,
                poolMemberMU.pdPoolMember.synced_particle,
            ),
        );
        return (index=clonedPoolMemberMU);
    }

    func realtime_balance_of_pool_member_mu{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(poolMemberMU: PDPoolMemberMU, time: felt) -> (balance: felt) {
        let (realtime_balance_of_wrapped_particle) = realtime_balance_of(
            poolMemberMU.pdPoolIndex.wrapped_particle, time
        );
        let (realtime_balance_of_synced_particle) = realtime_balance_of(
            poolMemberMU.pdPoolMember.synced_particle,
            poolMemberMU.pdPoolMember.synced_particle.rtb_settled_at,
        );
        let balance = (
            (realtime_balance_of_wrapped_particle - realtime_balance_of_synced_particle) *
            poolMemberMU.pdPoolMember.owned_unit
        ) + poolMemberMU.pdPoolMember.settled_value;
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

    func pool_member_update{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        poolMemberMU: PDPoolMemberMU, u_index: BasicParticle, unit: felt, time: felt
    ) -> (p_index: PDPoolIndex, pool_member: PDPoolMember, u_index: BasicParticle) {
        alloc_locals;
        let old_total_units = poolMemberMU.pdPoolIndex.total_units;
        let new_total_units = (old_total_units + unit) - poolMemberMU.pdPoolMember.owned_unit;
        let (settled_pool_index) = settle_for_pool_index(poolMemberMU.pdPoolIndex, time);
        let newPoolMemberMU = PDPoolMemberMU(settled_pool_index, poolMemberMU.pdPoolMember);
        let (settled_pool_member_mu) = settle_for_pool_member_mu(newPoolMemberMU, time);

        if (new_total_units != 0) {
            let nr = settled_pool_member_mu.pdPoolIndex.wrapped_particle.rtb_flow_rate;
            let er = 0;
            let r_mul_otu = nr * old_total_units;
            let (quotient, remainder) = unsigned_div_rem(r_mul_otu, new_total_units);
            let nr = quotient;
            let er = remainder;
            let (wp_with_new_fr, _) = setFlow1(
                nr, settled_pool_member_mu.pdPoolIndex.wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = setFlow1(
                settled_u_index.rtb_flow_rate + er, settled_u_index
            );

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember.settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        } else {
            let er = settled_pool_member_mu.pdPoolIndex.wrapped_particle.rtb_flow_rate *
                old_total_units;
            let nr = 0;
            let (wp_with_new_fr, _) = setFlow1(
                nr, settled_pool_member_mu.pdPoolIndex.wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = setFlow1(
                settled_u_index.rtb_flow_rate + er, settled_u_index
            );

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember.settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        }
    }

    func shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, value: felt, time: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex) {
        if (p_index.total_units != 0) {
            let nx = (value / p_index.total_units) * p_index.total_units;
            let (settled_u_index) = settle(u_index, time);
            let (shift1_on_u_index, _) = shift1(-nx, settled_u_index);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (shift1_on_wrapped_particle, _) = shift1(
                nx / p_index.total_units, settled_p_index.wrapped_particle
            );
            let newPoolIndex = PDPoolIndex(settled_p_index.total_units, shift1_on_wrapped_particle);
            return (u_index=shift1_on_u_index, p_index=newPoolIndex);
        } else {
            let (u_index) = settle(u_index, time);
            let (p_index) = settle_for_pool_index(p_index, time);
            return (u_index=u_index, p_index=p_index);
        }
    }

    func flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, value: felt, time: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex) {
        if (p_index.total_units != 0) {
            let nr = (value / p_index.total_units) * p_index.total_units;
            let (settled_u_index) = settle(u_index, time);
            let (shiftFlow1_on_u_index, _) = shiftFlow1(-nr, settled_u_index);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (shiftFlow1_on_wrapped_particle, _) = shiftFlow1(
                nr / p_index.total_units, settled_p_index.wrapped_particle
            );
            let _p_index = PDPoolIndex(p_index.total_units, shiftFlow1_on_wrapped_particle);
            return (u_index=shiftFlow1_on_u_index, p_index=_p_index);
        } else {
            let (settled_u_index) = settle(u_index, time);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (shiftFlow1_on_u_index, _) = shiftFlow1(0, settled_u_index);
            let (shiftFlow1_on_wrapped_particle, _) = shiftFlow1(
                0, settled_p_index.wrapped_particle
            );
            let newPoolIndex = PDPoolIndex(
                settled_p_index.total_units, shiftFlow1_on_wrapped_particle
            );
            return (u_index=shiftFlow1_on_u_index, p_index=newPoolIndex);
        }
    }
}
