%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.pools.library import Pool, PDPoolIndex, PDPoolMember

@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    distributor: felt
) {
    Pool.initializer(distributor);
    return ();
}

@view
func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    index: PDPoolIndex
) {
    return Pool.getIndex();
}

@view
func distributor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (address: felt) {
    return Pool.distributor();
}

@external
func setIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: PDPoolIndex) {
    Pool.setIndex(index);
    return ();
}

@view
func getMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(member: felt) -> (
    member_data: PDPoolMember
) {
    return Pool.getMember(member);
}

@external
func updatePoolMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    member: felt, unit: felt
) -> (success: felt) {
    return Pool.updatePoolMember(member, unit);
}
