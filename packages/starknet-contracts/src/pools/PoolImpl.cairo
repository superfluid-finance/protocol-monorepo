%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.starknet.common.syscalls import get_block_timestamp, get_caller_address

from src.pools.library import Pool, PDPoolIndex, PDPoolMember

@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(admin: felt) {
    Pool.initializer(admin);
    return ();
}

@view
func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    index: PDPoolIndex
) {
    return Pool.getIndex();
}

@view
func getPendingDistribution{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    value: felt
) {
    return Pool.getPendingDistribution();
}

@view
func getClaimable{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    time: felt, memberAddress: felt
) -> (value: felt) {
    return Pool.getClaimable(time, memberAddress);
}

@external
func updatePoolMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    member: felt, unit: felt
) -> (success: felt) {
    return Pool.updatePoolMember(member, unit);
}

@external
func claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    success: felt
) {
    let (timestamp) = get_block_timestamp();
    let (caller) = get_caller_address();
    return Pool._claimAll(timestamp, caller);
}

@external
func operatorSetIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    index: PDPoolIndex
) -> (success: felt) {
    return Pool.operatorSetIndex(index);
}

@external
func operatorConnectMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    memberAddress: felt, dbConnect: felt
) -> (success: felt) {
    let (timestamp) = get_block_timestamp();
    return Pool.operatorConnectMember(timestamp, memberAddress, dbConnect);
}
