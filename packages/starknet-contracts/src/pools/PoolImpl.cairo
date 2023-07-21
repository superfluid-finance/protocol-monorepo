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
func admin{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (address: felt) {
    return Pool.admin();
}

@view
func getIndex{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    index: PDPoolIndex
) {
    return Pool.getIndex();
}

@view
func getTotalUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    value: felt
) {
    return Pool.getTotalUnits();
}

@view
func getDisconnectedUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (unit: felt) {
    return Pool.getDisconnectedUnits();
}

@view
func getUnits{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    memberAddress: felt
) -> (value: felt) {
    return Pool.getUnits(memberAddress);
}

@view
func getDistributionFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    flow_rate: felt
) {
    return Pool.getDistributionFlowRate();
}

@view
func getConnectedFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (flowRate: felt) {
    return Pool.getConnectedFlowRate();
}

@view
func getDisconnectedFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (flowRate: felt) {
    return Pool.getDisconnectedFlowRate();
}

@view
func getDisconnectedBalance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(time: felt) -> (value: felt) {
    return Pool.getDisconnectedBalance(time);
}

@view
func getMemberFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    memberAddress: felt
) -> (flow_rate: felt) {
    return Pool.getMemberFlowRate(memberAddress);
}

@view
func getClaimable{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    time: felt, memberAddress: felt
) -> (value: felt) {
    return Pool.getClaimable(time, memberAddress);
}

@external
func updateMember{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    member: felt, unit: felt
) -> (success: felt) {
    return Pool.updateMember(member, unit);
}

@external
func claimAll{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    success: felt
) {
    let (caller) = get_caller_address();
    return Pool.claimAll(caller);
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
