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
    totalSupply: felt
) {
    return SuperToken.totalSupply();
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
    return SuperToken.balanceOf(account);
}

@view
func realtimeBalanceNow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (rtb: felt) {
    let (timestamp) = get_block_timestamp();
    return SuperToken.realtimeBalanceAt(account, timestamp);
}

@view
func realtimeBalanceAt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt, time: felt
) -> (rtb: felt) {
    return SuperToken.realtimeBalanceAt(account, time);
}

@view
func realtimeBalanceVectorNow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (own: felt, fromPool: felt, deposit: felt) {
    let (timestamp) = get_block_timestamp();
    return SuperToken.realtimeBalanceVectorAt(account, timestamp);
}

@view
func realtimeBalanceVectorAt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt, time: felt
) -> (own: felt, fromPool: felt, deposit: felt) {
    return SuperToken.realtimeBalanceVectorAt(account, time);
}

@view
func getNetFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (flowRate: felt) {
    return SuperToken.getNetFlowRate(account);
}

@view
func getFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    _from: felt, to: felt, flowId: felt
) -> (flowRate: felt) {
    return SuperToken.getFlowRate(_from, to, flowId);
}

@external
func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt, amount: felt) -> (success: felt) {
    alloc_locals;
    return SuperToken.transfer(to, amount);
}

@external
func transferFrom{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, to: felt, amount: felt) -> (success: felt) {
    alloc_locals;
    return SuperToken.transferFrom(_from, to, amount);
}

@external
func shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, recipient: felt, amount: felt
) -> (success: felt) {
    return SuperToken.shift(sender, recipient, amount);
}

@external
func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, recipient: felt, flowId: felt, flowRate: felt
) -> (success: felt) {
    return SuperToken.flow(sender, recipient, flowId, flowRate);
}

@external
func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, poolAddress: felt, reqAmount: felt
) -> (success: felt, actualAmount: felt) {
    return SuperToken.distribute(sender, poolAddress, reqAmount);
}

@external
func distributeFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, pool: felt, flowId: felt, reqFlowRate: felt
) -> (success: felt, actualFlowRate: felt, newDistributionFlowRate: felt) {
    return SuperToken.distributeFlow(sender, pool, flowId, reqFlowRate);
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
func getPoolAdjustmentFlowInfo{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt) -> (adjustmentRecipient: felt, flowHash: felt, flowRate: felt){
    return SuperToken.getPoolAdjustmentFlowInfo(pool);
}

@external
func appendIndexUpdateByPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(particle: BasicParticle, time: felt) -> (success: felt) {
    SuperToken.appendIndexUpdateByPool(particle, time);
    return (success=TRUE);
}

@external
func poolSettleClaim{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(claimRecipient: felt, amount: felt) -> (success: felt){
    SuperToken.poolSettleClaim(claimRecipient, amount);
    return (success=TRUE);
}

func isPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(address: felt) -> (success: felt) {
    return SuperToken.isPool(address);
}

@view
func getNumConnections{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    account: felt
) -> (value: felt) {
    return SuperToken.getNumConnections(account);
}

@external
func mint{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt, amount: felt) {
    SuperToken._mint(to, amount);
    return ();
}

