%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.math import assert_nn
from starkware.cairo.common.bool import TRUE

from openzeppelin.access.ownable.library import Ownable

from src.utils.SemanticMoney import (
    PDPoolIndex,
    PDPoolMember,
    PDPoolMemberMU,
    SemanticMoney,
    BasicParticle,
)
from src.interfaces.ISuperToken import ISuperToken

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
func Pool_claimed_values(member: felt) -> (values: felt) {
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
        Ownable.initializer(caller);
        Pool_admin.write(admin);
        return ();
    }

    func getPendingDistribution{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        ) -> (value: felt) {
        let (time) = get_block_timestamp();
        let (index) = Pool_index.read();
        let (realtime_balance) = SemanticMoney.realtime_balance_of(index.wrapped_particle, time);
        let (pendingUnits) = Pool_pending_units.read();
        let pendingDistribution = realtime_balance * pendingUnits;
        return (value=pendingDistribution);
    }

    func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        index: PDPoolIndex
    ) {
        let (index) = Pool_index.read();
        return (index=index);
    }

    func operatorSetIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (success: felt) {
        Ownable.assert_only_owner();
        Pool_index.write(index);
        return (success=TRUE);
    }

    func getMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        member: felt
    ) -> (member_data: PDPoolMember) {
        let (member_data) = Pool_members.read(member);
        return (member_data=member_data);
    }

    func updatePoolMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        member: felt, unit: felt
    ) -> (success: felt) {
        alloc_locals;
        with_attr error_message("Pool: unit is negative") {
            assert_nn(unit);
        }
        let (caller) = get_caller_address();
        let (admin) = Pool_admin.read();
        with_attr error_message("Pool: not the admin!") {
            assert caller = admin;
        }
        let (contract_address) = get_contract_address();
        let (connected) = ISuperToken.isMemberConnected(
            contract_address=caller, pool=contract_address, memberAddress=member
        );

        // update pool's pending units`
        if (connected == TRUE) {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(member);
            Pool_pending_units.write(pendingUnits - member_data.owned_unit + unit);

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
            let (owner) = Ownable.owner();
            let (absorbed) = ISuperToken.absorbParticleFromPool(
                contract_address=owner, account=admin, particle=particle
            );
            assert absorbed = TRUE;

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
            let (owner) = Ownable.owner();
            let (absorbed) = ISuperToken.absorbParticleFromPool(
                contract_address=owner, account=admin, particle=particle
            );
            assert absorbed = TRUE;

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
        let (claimed_values) = Pool_claimed_values.read(memberAddress);
        return (value=realtime_balance - claimed_values);
    }

    func _claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt
    ) -> (success: felt) {
        let (value) = getClaimable(time, memberAddress);
        let (owner) = Ownable.owner();
        let (contract_address) = get_contract_address();
        let (sent) = ISuperToken.shift(
            contract_address=owner,
            senderAddress=contract_address,
            receiverAddress=memberAddress,
            amount=value,
        );
        assert sent = TRUE;
        Pool_claimed_values.write(memberAddress, value);
        return (success=TRUE);
    }

    func claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        success: felt
    ) {
        let (timestamp) = get_block_timestamp();
        let (caller) = get_contract_address();
        return _claimAll(timestamp, caller);
    }

    func operatorConnectMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt, dbConnect: felt
    ) -> (success: felt) {
        Ownable.assert_only_owner();
        if (dbConnect == TRUE) {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(memberAddress);
            Pool_pending_units.write(pendingUnits - member_data.owned_unit);
        } else {
            let (pendingUnits) = Pool_pending_units.read();
            let (member_data) = Pool_members.read(memberAddress);
            Pool_pending_units.write(pendingUnits + member_data.owned_unit);
        }
        let (timestamp) = get_block_timestamp();
        _claimAll(timestamp, memberAddress);
        return (success=TRUE);
    }
}
