%lang starknet

from starkware.starknet.common.syscalls import get_caller_address, get_block_timestamp
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.uint256 import Uint256
from starkware.cairo.common.bool import TRUE

from openzeppelin.access.ownable.library import Ownable

from src.tokens.ERC20x.SuperToken.library import SuperToken, BasicParticle

@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    name: felt, symbol: felt, decimals: felt, pool_class_hash: felt
) {
    let (caller) = get_caller_address();
    SuperToken.initializer(name, symbol, decimals, pool_class_hash);
    return ();
}

//
// Getters
//

@view
func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
    return SuperToken.name();
}

@view
func symbol{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (symbol: felt) {
    return SuperToken.symbol();
}

@view
func totalSupply{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    total_supply: felt
) {
    return SuperToken.total_supply();
}

@view
func decimals{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    decimals: felt
) {
    return SuperToken.decimals();
}

@view
func balanceOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(account: felt) -> (
    balance: felt
) {
    return SuperToken.balance_of(account);
}

@view
func realtimeBalanceOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (rtb: felt) {
    return SuperToken.realtime_balance_of(account);
}

@view
func realtimeBalanceAt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt, time: felt
) -> (rtb: felt) {
    return SuperToken.realtime_balance_at(account, time);
}

@view
func realtimeBalanceVectorAt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt, time: felt
) -> (available: felt, deposit: felt) {
    return SuperToken.realtime_balance_vector_at(account, time);
}

@view
func getNetFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (flow_rate: felt) {
    return SuperToken.get_net_flow_rate(account);
}

@view
func getFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    _from: felt, to: felt, flowId: felt
) -> (flow_rate: felt) {
    return SuperToken.get_flow_rate(_from, to, flowId);
}

//
// Externals
//

@external
func shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, recipient: felt, amount: felt
) -> (success: felt) {
    return SuperToken.shift(sender, recipient, amount);
}

@external
func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, recipient: felt, flow_id: felt, flow_rate: felt
) -> (success: felt) {
    return SuperToken.flow(sender, recipient, flow_id, flow_rate);
}

@external
func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, poolAddress: felt, reqAmount: felt
) -> (success: felt, actualAmount: felt) {
    return SuperToken.distribute(sender, poolAddress, reqAmount);
}

@external
func distributeFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, poolAddress: felt, flow_id: felt, reqFlowRate: felt
) -> (success: felt, actualFlowRate: felt) {
    return SuperToken.distribute_flow(sender, poolAddress, flow_id, reqFlowRate);
}

@external
func connectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt) -> (
    success: felt
) {
    return SuperToken.connectPool(to);
}

@external
func disconnectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt) -> (
    success: felt
) {
    return SuperToken.disconnectPool(to);
}

@external
func createPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (pool: felt) {
    return SuperToken.createPool();
}

@view
func isMemberConnected{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    pool: felt, memberAddress: felt
) -> (success: felt) {
    return SuperToken.isMemberConnected(pool, memberAddress);
}

@view
func getNumConnections{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (value: felt) {
    return SuperToken.getNumConnections(account);
}

@external
func absorbParticleFromPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(account: felt, particle: BasicParticle) -> (success: felt) {
    return SuperToken.absorbParticleFromPool(account, particle);
}

@external
func mint{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt, amount: felt) {
    SuperToken._mint(to, amount);
    return ();
}

