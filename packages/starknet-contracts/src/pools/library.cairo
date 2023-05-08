%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.math import assert_nn, assert_not_zero
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.alloc import alloc

from openzeppelin.access.ownable.library import Ownable

from src.utils.SemanticMoney import (
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
    SemanticMoney,
    BasicParticle,
)
from src.interfaces.ISuperToken import ISuperToken
from src.interfaces.ISuperTokenPool import ISuperTokenPoolAdmin

@storage_var
func Pool_POOL_ADMIN() -> (address: felt) {
}

@storage_var
func Pool_admin() -> (address: felt) {
}

@storage_var
func Pool_index() -> (index: PDPoolIndex) {
}

@storage_var
func Pool_members(member: felt) -> (member_data: PDPoolMember) {
}

@storage_var
func Pool_claimed_values(member: felt) -> (value: felt) {
}

@storage_var
func Pool_pending_units() -> (value: felt) {
}

namespace Pool {
    //
    // Initializer
    //
    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(admin: felt) {
        let (caller) = get_caller_address();
        Pool_POOL_ADMIN.write(caller);
        Pool_admin.write(admin);
        return ();
    }

    func getPendingDistribution{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        ) -> (value: felt) {
        let (time) = get_block_timestamp();
        let (index) = Pool_index.read();
        let (rtb) = SemanticMoney.rtb_per_unit(index, time);
        let (pendingUnits) = Pool_pending_units.read();
        let pendingDistribution = rtb * pendingUnits;
        return (value=pendingDistribution);
    }

    func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        index: PDPoolIndex
    ) {
        return Pool_index.read();
    }

    func getTotalUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        value: felt
    ) {
        let (index) = Pool_index.read();
        return (value=index.total_units);
    }

    func getUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        memberAddress: felt
    ) -> (value: felt) {
        let (member_data) = Pool_members.read(memberAddress);
        return (value=member_data.owned_unit);
    }

    func getDistributionFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        ) -> (flow_rate: felt) {
        let (index) = Pool_index.read();
        let (flow_rate) = SemanticMoney.flow_rate_per_unit(index);
        return (flow_rate= flow_rate * index.total_units);
    }

    func getPendingDistributionFlowRate{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }() -> (flow_rate: felt) {
        let (index) = Pool_index.read();
        let (flow_rate) = SemanticMoney.flow_rate_per_unit(index);
        let (pendingUnits) = Pool_pending_units.read();
        return (flow_rate= flow_rate * pendingUnits);
    }

    func getMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        member: felt
    ) -> (member_data: PDPoolMember) {
        return Pool_members.read(member);
    }

    func getMemberFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        memberAddress: felt
    ) -> (flow_rate: felt) {
        let (member_data) = Pool_members.read(memberAddress);
        if (member_data.owned_unit == 0) {
            return (flow_rate=0);
        } else {
            let (index) = Pool_index.read();
            let (flow_rate) = SemanticMoney.flow_rate_per_unit(index);
            return (flow_rate=flow_rate * member_data.owned_unit);
        }
    }

    func getPendingUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        value: felt
    ) {
        return Pool_pending_units.read();
    }

    func operatorSetIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        let (pool_admin) = Pool_POOL_ADMIN.read();
        with_attr error_message("Pool: caller is not pool admin") {
            assert pool_admin = caller;
        }
        Pool_index.write(index);
        return (success=TRUE);
    }

    func updateMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        member: felt, unit: felt
    ) -> (success: felt) {
        alloc_locals;
        with_attr error_message("Pool: negative unit not allowed") {
            assert_nn(unit);
        }
        let (caller) = get_caller_address();
        let (admin) = Pool_admin.read();
        with_attr error_message("Pool: not the admin!") {
            assert caller = admin;
        }
        let (contract_address) = get_contract_address();
        let (pool_admin) = Pool_POOL_ADMIN.read();
        let (connected) = ISuperTokenPoolAdmin.isMemberConnected(
            contract_address=pool_admin, pool=contract_address, memberAddress=member
        );

        // update pool's pending units`
        if (connected == FALSE) {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(member);
            Pool_pending_units.write(pendingUnits - member_data.owned_unit + unit);

            // update pool member's units
            let (index) = Pool_index.read();
            let pd_member_mu = PDPoolMemberMU(index, member_data);
            let empty_particle = BasicParticle(0, 0, 0);
            let (timestamp) = get_block_timestamp();
            let (index, member_data, particle) = SemanticMoney.pool_member_update(
                pd_member_mu, empty_particle, unit, timestamp
            );
            Pool_index.write(index);
            Pool_members.write(member, member_data);

            ISuperTokenPoolAdmin.absorbParticleFromPool(
                contract_address=pool_admin,
                account=admin,
                particle=particle,
            );

            // additional side effects of triggering claimAll
            _claimAll(timestamp, member);
            return (success=TRUE);
        } else {
            // update pool member's units
            let (index) = Pool_index.read();
            let (member_data) = Pool_members.read(member);
            let pd_member_mu = PDPoolMemberMU(index, member_data);
            let empty_particle = BasicParticle(0, 0, 0);
            let (timestamp) = get_block_timestamp();
            let (index, member_data, particle) = SemanticMoney.pool_member_update(
                pd_member_mu, empty_particle, unit, timestamp
            );
            Pool_index.write(index);
            Pool_members.write(member, member_data);

            ISuperTokenPoolAdmin.absorbParticleFromPool(
                contract_address=pool_admin,
                account=admin,
                particle=particle,
            );

            // additional side effects of triggering claimAll
            _claimAll(timestamp, member);
            return (success=TRUE);
        }
    }

    func getClaimable{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt
    ) -> (value: felt) {
        let (index) = Pool_index.read();
        let (member_data) = Pool_members.read(memberAddress);
        let pd_member_mu = PDPoolMemberMU(index, member_data);
        let (realtime_balance) = SemanticMoney.realtime_balance_of_pool_member_mu(
            pd_member_mu, time
        );
        let (claimed_value) = Pool_claimed_values.read(memberAddress);
        let claimable = realtime_balance - claimed_value;
        return (value=claimable);
    }

    func _claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt
    ) -> (success: felt) {
        alloc_locals;
        let empty_particle = BasicParticle(0, 0, 0);
        let (value) = getClaimable(time, memberAddress);
        let (a, b) = SemanticMoney.shift2(empty_particle, empty_particle, value);

        let (contract_address) = get_contract_address();

        let (pool_admin) = Pool_POOL_ADMIN.read();

        ISuperTokenPoolAdmin.absorbParticleFromPool(
            contract_address=pool_admin,
            account=contract_address,
            particle=a,
        );
        ISuperTokenPoolAdmin.absorbParticleFromPool(
            contract_address=pool_admin,
            account=memberAddress,
            particle=b,
        );

        let (initialClaimedValue) = Pool_claimed_values.read(memberAddress);
        Pool_claimed_values.write(memberAddress, value + initialClaimedValue);

        return (success = TRUE);
    }

    func claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        success: felt
    ) {
        alloc_locals;
        let (timestamp) = get_block_timestamp();
        let (caller) = get_contract_address();
        return _claimAll(timestamp, caller);
    }

    func operatorConnectMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt, dbConnect: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (pool_admin) = Pool_POOL_ADMIN.read();
        with_attr error_message("Pool: caller is not pool admin") {
            assert pool_admin = caller;
        }
        if (dbConnect == TRUE) {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(memberAddress);
            Pool_pending_units.write(pendingUnits - member_data.owned_unit);
        } else {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(memberAddress);
            Pool_pending_units.write(pendingUnits + member_data.owned_unit);
        }
        _claimAll(time, memberAddress);
        return (success=TRUE);
    }
}
