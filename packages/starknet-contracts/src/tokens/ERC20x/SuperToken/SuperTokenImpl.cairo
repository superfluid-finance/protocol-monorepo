%lang starknet

from starkware.starknet.common.syscalls import get_caller_address, get_block_timestamp
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.uint256 import Uint256
from starkware.cairo.common.bool import TRUE

from openzeppelin.access.ownable.library import Ownable

from src.tokens.ERC20x.SuperToken.library import SuperToken


@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    name: felt, symbol: felt, decimals: felt, initial_supply: felt
) {
    let (caller) = get_caller_address();
    SuperToken.initializer(name, symbol, decimals);
    SuperToken._mint(caller, initial_supply);
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
func allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    owner: felt, spender: felt
) -> (remaining: felt) {
    return SuperToken.allowance(owner, spender);
}

//
// Externals
//

@external
func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    recipient: felt, amount: felt
) -> (success: felt) {
    return SuperToken.transfer(recipient, amount);
}

@external
func transferFrom{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    sender: felt, recipient: felt, amount: felt
) -> (success: felt) {
    return SuperToken.transfer_from(sender, recipient, amount);
}

@external
func mint{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    to: felt, amount: felt
) {
    SuperToken._mint(to, amount);
    return ();
}

@external
func createFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) -> (success: felt) {
    SuperToken.createFlow(receiver, flow_rate);
    return (success=TRUE);
}

@external
func updateFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) -> (success: felt) {
    SuperToken.updateFlow(receiver, flow_rate);
    return (success=TRUE);
}

@external
func approve{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, amount: felt
) -> (success: felt) {
    SuperToken.approve(spender, amount);
    return (success=TRUE);
}

@external
func increase_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, added_value: felt
) -> (success: felt) {
    SuperToken.increase_allowance(spender, added_value);
    return (success=TRUE);
}

@external
func decrease_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, subtracted_value: felt
) -> (success: felt) {
    SuperToken.decrease_allowance(spender, subtracted_value);
    return (success=TRUE);
}
