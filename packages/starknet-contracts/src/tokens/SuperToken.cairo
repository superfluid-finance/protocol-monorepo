// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v0.6.0 (token/erc20/presets/ERC20Mintable.cairo)

%lang starknet

from starkware.starknet.common.syscalls import get_caller_address, get_block_timestamp
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.uint256 import Uint256
from starkware.cairo.common.bool import TRUE

from openzeppelin.access.ownable.library import Ownable

from src.tokens.library import SuperERC20


@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    name: felt, symbol: felt, decimals: felt, initial_supply: felt, recipient: felt, owner: felt
) {
    SuperERC20.initializer(name, symbol, decimals);
    SuperERC20._mint(recipient, initial_supply);
    return ();
}

//
// Getters
//

@view
func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
    return SuperERC20.name();
}

@view
func symbol{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (symbol: felt) {
    return SuperERC20.symbol();
}

@view
func totalSupply{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    total_supply: felt
) {
    return SuperERC20.total_supply();
}

@view
func decimals{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    decimals: felt
) {
    return SuperERC20.decimals();
}

@view
func balanceOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(account: felt) -> (
    balance: felt
) {
    return SuperERC20.balance_of(account);
}

@view
func allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    owner: felt, spender: felt
) -> (remaining: felt) {
    return SuperERC20.allowance(owner, spender);
}

//
// Externals
//

// @external
// func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     recipient: felt, amount: Uint256
// ) -> (success: felt) {
//     return ERC20.transfer(recipient, amount);
// }

// @external
// func transferFrom{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
//     sender: felt, recipient: felt, amount: Uint256
// ) -> (success: felt) {
//     return ERC20.transfer_from(sender, recipient, amount);
// }

@external
func mint{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    to: felt, amount: felt
) {
    SuperERC20._mint(to, amount);
    return ();
}

@external
func createFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) -> (success: felt) {
    SuperERC20.createFlow(receiver, flow_rate);
    return (success=TRUE);
}

@external
func updateFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) -> (success: felt) {
    SuperERC20.updateFlow(receiver, flow_rate);
    return (success=TRUE);
}