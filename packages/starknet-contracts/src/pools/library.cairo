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
from src.interfaces.ISuperfluidPool import ISuperfluidPoolOperator

@storage_var
func Pool_POOL_OPERATOR() -> (address: felt) {
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
func Pool_disconnectedMembers() -> (pdMember: PDPoolMember) {
}

@storage_var
func Pool_claimedByDisconnectedMembers() -> (value: felt) {
}

namespace Pool {
    //
    // Initializer
    //
    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(admin: felt) {
        let (caller) = get_caller_address();
        Pool_POOL_OPERATOR.write(caller);
        Pool_admin.write(admin);
        return ();
    }

    func admin{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (address: felt) {
        return Pool_admin.read();
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

    func getDisconnectedUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        unit: felt
    ) {
        let (pdMember) = Pool_disconnectedMembers.read();
        return (unit=pdMember.owned_unit);
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
        let (flow_rate) = SemanticMoney.flow_rate_for_pool_index(index);
        return (flow_rate=flow_rate);
    }

    func getConnectedFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (flowRate: felt) {
        let (index) = Pool_index.read();
        let (flowRate) = SemanticMoney.flow_rate_for_pool_index(index);
        return (flowRate=flowRate);
    }

    func getDisconnectedFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        ) -> (flowRate: felt) {
        let (index) = Pool_index.read();
        let (flowRate) = SemanticMoney.flow_rate_per_unit(index);
        let (disconnectedPDMember) = Pool_disconnectedMembers.read();
        return (flowRate=flowRate * disconnectedPDMember.owned_unit);
    }

    func getDisconnectedBalance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(time: felt) -> (value: felt) {
        let (disconnectedPDMember) = Pool_disconnectedMembers.read();
        let (index) = Pool_index.read();
        let pdMemberMu = PDPoolMemberMU(index, disconnectedPDMember);
        let (balance) = SemanticMoney.realtime_balance_of_pool_member_mu(pdMemberMu, time);
        let (claimedByDisconnectedMembers) = Pool_claimedByDisconnectedMembers.read();
        return (value = balance - claimedByDisconnectedMembers);
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

    func getClaimable{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt
    ) -> (value: felt) {
        let (index) = Pool_index.read();
        let (member_data) = Pool_members.read(memberAddress);
        let pdMemberMu = PDPoolMemberMU(index, member_data);
        let (realtime_balance) = SemanticMoney.realtime_balance_of_pool_member_mu(
            pdMemberMu, time
        );
        let (claimed_value) = Pool_claimed_values.read(memberAddress);
        let claimable = realtime_balance - claimed_value;
        return (value=claimable);
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
        let (pool_operator) = Pool_POOL_OPERATOR.read();
        let (connected) = ISuperfluidPoolOperator.isMemberConnected(
            contract_address=pool_operator, pool=contract_address, memberAddress=member
        );
        let (index) = Pool_index.read();
        let (pdMember) = Pool_members.read(member);
        let (time) = get_block_timestamp();

        let pd_member_mu = PDPoolMemberMU(index, pdMember);
        let empty_particle = BasicParticle(0, 0, 0);

        if (connected == FALSE) {
            // trigger the side effect of claiming all if not connected
            let (claimed_amount) = _claimAll(time, member);
            // update pool's disconnected units
            _shiftDisconnectedUnits(unit - pdMember.owned_unit, claimed_amount, time);

            let (index, pdMember, p) = SemanticMoney.pool_member_update(pd_member_mu, empty_particle, unit, time);
            Pool_index.write(index);
            Pool_members.write(member, pdMember);
            let (success) = ISuperfluidPoolOperator.appendIndexUpdateByPool(contract_address=pool_operator, particle=p, time=time);
        } else {
            let (index, pdMember, p) = SemanticMoney.pool_member_update(pd_member_mu, empty_particle, unit, time);
            Pool_index.write(index);
            Pool_members.write(member, pdMember);
            let (success) = ISuperfluidPoolOperator.appendIndexUpdateByPool(contract_address=pool_operator, particle=p, time=time);
        }
        return (success=TRUE);
    }

    func claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(memberAddress: felt) -> (
        success: felt
    ) {
        alloc_locals;
        let (pool_operator) = Pool_POOL_OPERATOR.read();
        let (contract_address) = get_contract_address();
        let (connected) = ISuperfluidPoolOperator.isMemberConnected(
            contract_address=pool_operator, pool=contract_address, memberAddress=memberAddress
        );
        let (time) = get_block_timestamp();
        let (claimed_amount) = _claimAll(time, memberAddress);
        if (connected == FALSE) {
            _shiftDisconnectedUnits(0, claimed_amount, time);
            return (success=TRUE);
        } else {
            return (success=TRUE);
        }
    }

    func _claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt
    ) -> (value: felt) {
        alloc_locals;
        let (value) = getClaimable(time, memberAddress);
        let (pool_operator) = Pool_POOL_OPERATOR.read();

        let (success) = ISuperfluidPoolOperator.poolSettleClaim(contract_address=pool_operator, claimRecipient=memberAddress, amount=value);
        assert success = TRUE;

        let (initialClaimedValue) = Pool_claimed_values.read(memberAddress);
        Pool_claimed_values.write(memberAddress, value + initialClaimedValue);

        return (value = value);
    }

    func operatorSetIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        let (pool_operator) = Pool_POOL_OPERATOR.read();
        with_attr error_message("Pool: caller is not pool admin") {
            assert pool_operator = caller;
        }
        Pool_index.write(index);
        return (success=TRUE);
    }

    func operatorConnectMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        time: felt, memberAddress: felt, dbConnect: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (pool_operator) = Pool_POOL_OPERATOR.read();
        with_attr error_message("Pool: caller is not pool operator") {
            assert pool_operator = caller;
        }

        // NB! This is an assumption that isConnected = !doConnect,
        //     and it should be respected by the operator.

        // trigger the side effects of claiming all
        let (claimed_amount) = _claimAll(time, memberAddress);
        let (pdMember) = Pool_members.read(memberAddress);
        let (time) = get_block_timestamp();
        if (dbConnect == TRUE) {
            // previous disconnected, now to be connected
            // => removing from the disconnected distribution group
            _shiftDisconnectedUnits(-pdMember.owned_unit, claimed_amount, time);
        } else {
            // previous connected, now to be disconnected
            // => adding to disconnected distribution group
            _shiftDisconnectedUnits(pdMember.owned_unit, 0, time);
        }
        return (success=TRUE);
    }

    func _shiftDisconnectedUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(shift_units: felt, claimed_amount: felt, time: felt){
        let (index) = Pool_index.read();
        let (disconnectedPDMembers) = Pool_disconnectedMembers.read();
        let pdMemberMu = PDPoolMemberMU(index, disconnectedPDMembers);
        let (settledPoolMemberMu) = SemanticMoney.settle_for_pool_member_mu(pdMemberMu, time);
        let new_disconnectedMembers = PDPoolMember(settledPoolMemberMu.pdPoolMember.owned_unit + shift_units, settledPoolMemberMu.pdPoolMember._settled_value, settledPoolMemberMu.pdPoolMember._synced_particle);
        Pool_disconnectedMembers.write(new_disconnectedMembers);
        let (claimedByDisconnectedMembers) = Pool_claimedByDisconnectedMembers.read();
        Pool_claimedByDisconnectedMembers.write(claimedByDisconnectedMembers + claimed_amount);
        return ();
    }

}
