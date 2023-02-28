%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import get_caller_address, get_block_timestamp
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
func Pool_distributor() -> (address: felt) {
}

@storage_var
func Pool_index() -> (index: PDPoolIndex) {
}

@storage_var
func Pool_members(member: felt) -> (member_data: PDPoolMember) {
}

namespace Pool {
    //
    // Initializer
    //
    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        distributor: felt
    ) {
        let (caller) = get_caller_address();
        Ownable.initializer(caller);
        Pool_distributor.write(distributor);
        return ();
    }

    func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        index: PDPoolIndex
    ) {
        Ownable.assert_only_owner();
        let (index) = Pool_index.read();
        return (index=index);
    }

    func setIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        index: PDPoolIndex
    ) {
        Ownable.assert_only_owner();
        Pool_index.write(index);
        return ();
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
        let (distributor) = Pool_distributor.read();
        with_attr error_message("Pool: not the distributor!") {
            assert_nn(caller - distributor);
            assert_nn(distributor - caller);
        }
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
        ISuperToken.absorb(contract_address=owner, account=distributor, particle=particle);
        return (success=TRUE);
    }
}
