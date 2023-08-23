%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math_cmp import is_le, is_not_zero, is_nn
from starkware.cairo.common.bool import TRUE

from src.utils.MathLib import MathLib

struct BasicParticle {
    _settled_at: felt,
    _settled_value: felt,
    _flow_rate: felt,
}

struct PDPoolIndex {
    total_units: felt,
    _wrapped_particle: BasicParticle,  // This value is measured per unit
}

struct PDPoolMember {
    owned_unit: felt,
    _settled_value: felt,
    _synced_particle: BasicParticle,  // It is a copy of the wrapped_particle of the index at the time an operation is performed.
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
        let a_is_less_than_or_equal_to_b = is_le(a._settled_at, b._settled_at);
        if (a_is_less_than_or_equal_to_b == TRUE) {
            let (settledBasicParticleForA) = settle(a, b._settled_at);
            let (settledBasicParticleForB) = settle(b, b._settled_at);
            let c = BasicParticle(
                b._settled_at,
                settledBasicParticleForA._settled_value + settledBasicParticleForB._settled_value,
                a._flow_rate + b._flow_rate,
            );
            return (c=c);
        } else {
            let (settledBasicParticleForA) = settle(a, a._settled_at);
            let (settledBasicParticleForB) = settle(b, a._settled_at);
            let c = BasicParticle(
                a._settled_at,
                settledBasicParticleForA._settled_value + settledBasicParticleForB._settled_value,
                a._flow_rate + b._flow_rate,
            );
            return (c=c);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    //////////// Basic Particle Operations
    ////////////////////////////////////////////////////////////////////////////////////////

    func clone{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: BasicParticle) -> (b: BasicParticle) {
        let clonedBasicParticle = BasicParticle(a._settled_at, a._settled_value, a._flow_rate);
        return (b=clonedBasicParticle);
    }

    func settled_at{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: BasicParticle) -> (time: felt) {
        return (time=a._settled_at);
    }

    func flow_rate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: BasicParticle) -> (flow_rate: felt) {
        return (flow_rate=a._flow_rate);
    }

    func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (index: BasicParticle) {
        let timeDelta = time - index._settled_at;
        let newSettledValue = (timeDelta * index._flow_rate) + index._settled_value;
        let newBasicParticle = BasicParticle(time, newSettledValue, index._flow_rate);
        return (index=newBasicParticle);
    }

    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: BasicParticle, time: felt
    ) -> (balance: felt) {
        let timeDelta = time - index._settled_at;
        let balance = (timeDelta * index._flow_rate) + index._settled_value;
        return (balance=balance);
    }

    func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        amount: felt, index: BasicParticle
    ) -> (index: BasicParticle, amount: felt) {
        let newBasicParticle = BasicParticle(
            index._settled_at, index._settled_value + amount, index._flow_rate
        );
        return (index=newBasicParticle, amount=amount);
    }

    func flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        flow_rate: felt, index: BasicParticle
    ) -> (index: BasicParticle, flow_rate: felt) {
        let newBasicParticle = BasicParticle(index._settled_at, index._settled_value, flow_rate);
        return (index=newBasicParticle, flow_rate=flow_rate);
    }

    func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: BasicParticle, amount: felt
    ) -> (a: BasicParticle, b: BasicParticle) {
        let (bBasicParticle, _) = shift1(amount, b);
        let (aBasicParticle, _) = shift1(-amount, a);
        return (a=aBasicParticle, b=bBasicParticle);
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
        let (_, b1) = flow2(a, mempty, a._flow_rate, time);
        let (m, b2) = flow2(a, mempty, -a._flow_rate + flow_rate, time);
        let (b_and_b1) = mappend(b, b1);
        let (n) = mappend(b_and_b1, b2);
        return (m=m, n=n);
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    //////////// Proportional Distribution Pool Index Operations
    ////////////////////////////////////////////////////////////////////////////////////////

    func clone_of_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (index: PDPoolIndex) {
        let clonedPoolIndex = PDPoolIndex(
            index.total_units,
            BasicParticle(index.wrapped_particle._settled_at, index._settled_value, index._flow_rate),
        );
        return (index=clonedPoolIndex);
    }

    func settled_at_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (time: felt) {
        let (time) = settled_at(index._wrapped_particle);
        return (time=time);
    }

    func settle_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex, time: felt
    ) -> (index: PDPoolIndex) {
        let (settled_wrapped_particle) = settle(index._wrapped_particle, time);
        let newPoolIndex = PDPoolIndex(index.total_units, settled_wrapped_particle);
        return (index=newPoolIndex);
    }

    func flow_rate_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: PDPoolIndex) -> (flow_rate: felt){
        return (flow_rate=a._wrapped_particle._flow_rate * a.total_units);
    }

    func flow_rate_per_unit{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: PDPoolIndex) -> (flow_rate: felt) {
        let (fr) = flow_rate(a._wrapped_particle);
        return (flow_rate=fr);
    }

    func shift1_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: PDPoolIndex, value: felt) -> (b:PDPoolIndex, value: felt) {
        alloc_locals;
        let zero_status = is_not_zero(a.total_units);
        if(zero_status == TRUE) {
            let (quotient, _) = MathLib.div(value, a.total_units);
            let actualValue = quotient * a.total_units;
            let (newWrappedParticle, _) = shift1(actualValue/a.total_units, a._wrapped_particle);
            let newPoolIndex = PDPoolIndex(a.total_units, newWrappedParticle);
            return (b=newPoolIndex, value=actualValue);
        }
        return (b=a, value=0);
    }

    func flow1_for_pool_index{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: PDPoolIndex, value: felt) -> (b:PDPoolIndex, flow_rate: felt) {
        alloc_locals;
        let zero_status = is_not_zero(a.total_units);
        let is_not_negative = is_nn(value);
        if(zero_status == TRUE) {
            let (quotient, _) = MathLib.div(value, a.total_units);
            let actualFlowRate = quotient * a.total_units;
            let (newWrappedParticle, _) = flow1(actualFlowRate/a.total_units, a._wrapped_particle);
            let newPoolIndex = PDPoolIndex(a.total_units, newWrappedParticle);
            return (b=newPoolIndex, flow_rate=actualFlowRate);
        }
        return (b=a, flow_rate=0);
    }

    ////////////////////////////////////////////////////////////////////////////////////////
    //////////// Proportional Distribution Pool Member Operations
    ////////////////////////////////////////////////////////////////////////////////////////

    func clone_of_pool_member_mu{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        poolMemberMU: PDPoolMemberMU
    ) -> (poolMemberMU: PDPoolMemberMU) {
        let clonedPoolMemberMU = PDPoolMemberMU(
            PDPoolIndex(
                poolMemberMU.pdPoolIndex.total_units, poolMemberMU.pdPoolIndex._wrapped_particle
            ),
            PDPoolMember(
                poolMemberMU.pdPoolMember.owned_unit,
                poolMemberMU.pdPoolMember._settled_value,
                poolMemberMU.pdPoolMember._synced_particle,
            ),
        );
        return (index=clonedPoolMemberMU);
    }

    func realtime_balance_of_pool_member_mu{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(poolMemberMU: PDPoolMemberMU, time: felt) -> (balance: felt) {
        let (realtime_balance_of_wrapped_particle) = realtime_balance_of(
            poolMemberMU.pdPoolIndex._wrapped_particle, time
        );
        let (realtime_balance_of_synced_particle) = realtime_balance_of(
            poolMemberMU.pdPoolMember._synced_particle,
            poolMemberMU.pdPoolMember._synced_particle._settled_at,
        );
        let balance = (
            (realtime_balance_of_wrapped_particle - realtime_balance_of_synced_particle) *
            poolMemberMU.pdPoolMember.owned_unit
        ) + poolMemberMU.pdPoolMember._settled_value;
        return (balance=balance);
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
                newSettledPoolIndex._wrapped_particle,
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
            let nr = settled_pool_member_mu.pdPoolIndex._wrapped_particle._flow_rate;
            let er = 0;
            let r_mul_otu = nr * old_total_units;
            let (quotient, remainder) = MathLib.div(r_mul_otu, new_total_units);
            let nr = quotient;
            let er = remainder;
            let (wp_with_new_fr, _) = flow1(
                nr, settled_pool_member_mu.pdPoolIndex._wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = flow1(settled_u_index._flow_rate + er, settled_u_index);

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember._settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        } else {
            let er = settled_pool_member_mu.pdPoolIndex._wrapped_particle._flow_rate *
                old_total_units;
            let nr = 0;
            let (wp_with_new_fr, _) = flow1(
                nr, settled_pool_member_mu.pdPoolIndex._wrapped_particle
            );
            let (settled_u_index) = settle(u_index, time);
            let (u_index_with_new_fr, _) = flow1(settled_u_index._flow_rate + er, settled_u_index);

            let newPoolIndex = PDPoolIndex(new_total_units, wp_with_new_fr);
            let newPoolMember = PDPoolMember(
                unit, settled_pool_member_mu.pdPoolMember._settled_value, wp_with_new_fr
            );
            return (p_index=newPoolIndex, pool_member=newPoolMember, u_index=u_index_with_new_fr);
        }
    }

    func shift2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, value: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex, actualAmount: felt) {
        alloc_locals;
        let (newPoolIndex, actualAmount) = shift1_for_pool_index(p_index, value);
        let (newBasicParticle, _) = shift1(-actualAmount, u_index);
        return (u_index=newBasicParticle, p_index=newPoolIndex, actualAmount=actualAmount);

    }

    func flow2_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        u_index: BasicParticle, p_index: PDPoolIndex, flow_rate: felt, time: felt
    ) -> (u_index: BasicParticle, p_index: PDPoolIndex, actualFlowRate: felt) {
        alloc_locals;
        let (settled_u_index) = settle(u_index, time);
        let (settled_p_index) = settle_for_pool_index(p_index, time);
        let (newPoolIndex, actualFlowRate) = flow1_for_pool_index(settled_p_index, flow_rate);
        let (newBasicParticle, _) = flow1(-actualFlowRate, settled_u_index);
        return (u_index=newBasicParticle, p_index=newPoolIndex, actualFlowRate=actualFlowRate);

    }

    func shiftFlow_pd{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: BasicParticle, b: PDPoolIndex, flow_rate: felt, time: felt
    ) -> (m: BasicParticle, n: PDPoolIndex, actualFlowRate: felt) {
        alloc_locals;
        let mempty = BasicParticle(0, 0, 0);
        let (_flow_rate) = flow_rate_for_pool_index(b);
        let (a1, _, _) = flow2_pd(mempty, b, -_flow_rate, time);
        let (a2, n, actualFlowRate) = flow2_pd(mempty, b, _flow_rate + flow_rate, time);
        let (a_and_a1) = mappend(a, a1);
        let (m) = mappend(a_and_a1, a2);
        return (m=m, n=n, actualFlowRate=actualFlowRate);
    }
}
