%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem
from starkware.cairo.common.math_cmp import is_le
from starkware.cairo.common.bool import TRUE

struct BasicParticle {
    settled_at: felt,
    settled_value: felt,
    flow_rate: felt,
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
        let a_is_less_than_or_equal_to_b = is_le(a.settled_at, b.settled_at);
        if (a_is_less_than_or_equal_to_b == TRUE) {
            let (settledBasicParticleForA) = settle(a, b.settled_at);
            let (settledBasicParticleForB) = settle(b, b.settled_at);
            let c = BasicParticle(
                b.settled_at,
                settledBasicParticleForA.settled_value + settledBasicParticleForB.settled_value,
                a.flow_rate + b.flow_rate,
            );
            return (c=c);
        } else {
            let (settledBasicParticleForA) = settle(a, a.settled_at);
            let (settledBasicParticleForB) = settle(b, a.settled_at);
            let c = BasicParticle(
                a.settled_at,
                settledBasicParticleForA.settled_value + settledBasicParticleForB.settled_value,
                a.flow_rate + b.flow_rate,
            );
            return (c=c);
        }
    }

    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (balance: felt) {
        let timeDelta = time - index.settled_at;
        let balance = (timeDelta * index.flow_rate) + index.settled_value;
        return (balance=balance);
    }

    func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (index: BasicParticle) {
        let timeDelta = time - index.settled_at;
        let newSettledValue = (timeDelta * index.flow_rate) + index.settled_value;
        let newBasicParticle = BasicParticle(time, newSettledValue, index.flow_rate);
        return (index=newBasicParticle);
    }

    func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        amount: felt, index: BasicParticle
    ) -> (index: BasicParticle, amount: felt) {
        let newBasicParticle = BasicParticle(
            index.settled_at, index.settled_value + amount, index.flow_rate
        );
        return (index=newBasicParticle, amount=amount);
    }

    func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, amount: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (bBasicParticle, _) = shift1(amount, b);
        let (aBasicParticle, _) = shift1(-amount, a);
        return (a=aBasicParticle, b=bBasicParticle);
    }

    func flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        flow_rate: felt, index: BasicParticle
    ) -> (index: BasicParticle, flow_rate: felt) {
        let newBasicParticle = BasicParticle(index.settled_at, index.settled_value, flow_rate);
        return (index=newBasicParticle, flow_rate=flow_rate);
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, flow_rate: felt, time: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (settledBasicParticleForA) = settle(a, time);
        let (settledBasicParticleForB) = settle(b, time);
        let (bBasicParticle, _) = flow1(flow_rate, settledBasicParticleForB);
        let (aBasicParticle, _) = flow1(-flow_rate, settledBasicParticleForA);
        return (a=aBasicParticle, b=bBasicParticle);
    }

    func shiftFlow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, flow_rate: felt, time: felt
    ) -> (m: BasicParticle, n: BasicParticle) {
        alloc_locals;
        let mempty = BasicParticle(0, 0, 0);
        let (_, b1) = flow2(a, mempty, a.flow_rate, time);
        let (m, b2) = flow2(a, mempty, -a.flow_rate + flow_rate, time);
        let (b_and_b1) = mappend(b, b1);
        let (n) = mappend(b_and_b1, b2);
        return (m=m, n=n);
    }

    // Pool Operations

    func clone_of_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (index: PDPoolIndex) {
        let clonedPoolIndex = PDPoolIndex(
            index.total_units,
            BasicParticle(index.wrapped_particle.settled_at, index.settled_value, index.flow_rate),
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
            poolMemberMU.pdPoolMember.synced_particle.settled_at,
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
        let (newSettledPoolIndex) = settle_for_pool_index(poolMemberMU.pdPoolIndex, time);
        let (new_settled_value) = realtime_balance_of_pool_member_mu(poolMemberMU, time);
        let newPoolMemberMU = PDPoolMemberMU(
            newSettledPoolIndex,
            PDPoolMember(
                poolMemberMU.pdPoolMember.owned_unit,
                new_settled_value,
                newSettledPoolIndex.wrapped_particle,
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
        let (settled_pool_member_mu) = settle_for_pool_member_mu(poolMemberMU, time);

        if (new_total_units != 0) {
            let nr = settled_pool_member_mu.pdPoolIndex.wrapped_particle.flow_rate;
            let er = 0;
            let r_mul_otu = nr * old_total_units;
            let (quotient, remainder) = unsigned_div_rem(r_mul_otu, new_total_units);
            let nr = quotient;
            let er = remainder;
            let (wp_with_new_fr, _) = flow1(
                nr, settled_pool_member_mu.pdPoolIndex.wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = flow1(settled_u_index.flow_rate + er, settled_u_index);

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember.settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        } else {
            let er = settled_pool_member_mu.pdPoolIndex.wrapped_particle.flow_rate *
                old_total_units;
            let nr = 0;
            let (wp_with_new_fr, _) = flow1(
                nr, settled_pool_member_mu.pdPoolIndex.wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = flow1(settled_u_index.flow_rate + er, settled_u_index);

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember.settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        }
    }

    func shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, value: felt, time: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex, actualAmount: felt) {
        if (p_index.total_units != 0) {
            let (quotient, _) = unsigned_div_rem(value, p_index.total_units);
            let nx = (quotient * p_index.total_units);
            let (settled_u_index) = settle(u_index, time);
            let (shift1_on_u_index, _) = shift1(-nx, settled_u_index);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (shift1_on_wrapped_particle, _) = shift1(
                nx / p_index.total_units, settled_p_index.wrapped_particle
            );
            let newPoolIndex = PDPoolIndex(settled_p_index.total_units, shift1_on_wrapped_particle);
            return (u_index=shift1_on_u_index, p_index=newPoolIndex, actualAmount=nx);
        } else {
            let (u_index) = settle(u_index, time);
            let (p_index) = settle_for_pool_index(p_index, time);
            return (u_index=u_index, p_index=p_index, actualAmount=0);
        }
    }

    func flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, value: felt, time: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex, actualFlowRate: felt) {
        if (p_index.total_units != 0) {
            let (quotient, _) = unsigned_div_rem(value, p_index.total_units);
            let nr = quotient * p_index.total_units;
            let (settled_u_index) = settle(u_index, time);
            let (flow1_on_u_index, _) = flow1(-nr, settled_u_index);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (flow1_on_wrapped_particle, _) = flow1(
                nr / p_index.total_units, settled_p_index.wrapped_particle
            );
            let _p_index = PDPoolIndex(p_index.total_units, flow1_on_wrapped_particle);
            return (u_index=flow1_on_u_index, p_index=_p_index, actualFlowRate=nr);
        } else {
            let (settled_u_index) = settle(u_index, time);
            let (settled_p_index) = settle_for_pool_index(p_index, time);
            let (flow1_on_u_index, _) = flow1(0, settled_u_index);
            let (flow1_on_wrapped_particle, _) = flow1(0, settled_p_index.wrapped_particle);
            let newPoolIndex = PDPoolIndex(settled_p_index.total_units, flow1_on_wrapped_particle);
            return (u_index=flow1_on_u_index, p_index=newPoolIndex, actualFlowRate=0);
        }
    }

    func shiftFlow_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: PDPoolIndex, flow_rate: felt, time: felt
    ) -> (m: BasicParticle, n: PDPoolIndex, actualFlowRate: felt) {
        alloc_locals;
        let mempty = BasicParticle(0, 0, 0);
        let _flow_rate = b.wrapped_particle.flow_rate * b.total_units;
        let (a1, _, _) = flow2_pd(mempty, b, -_flow_rate, time);
        let (a2, n, actualFlowRate) = flow2_pd(mempty, b, _flow_rate + flow_rate, time);
        let (a_and_a1) = mappend(a, a1);
        let (m) = mappend(a_and_a1, a2);
        return (m=m, n=n, actualFlowRate=actualFlowRate);
    }
}
