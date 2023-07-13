%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.examples.aqueduct.library import Aqueduct

@constructor
func constructor{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    tokenL: felt, tokenR: felt
) {
    Aqueduct.initializer(tokenL, tokenR);
    return ();
}

@view
func token1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    token: felt
) {
    return Aqueduct.token1();
}

@view
func token2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
    token: felt
) {
    return Aqueduct.token2();
}

@view
func pool1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (pool: felt) {
    return Aqueduct.pool1();
}

@view
func pool2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (pool: felt) {
    return Aqueduct.pool2();
}

@external
func onFlowUpdate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(token: felt, _from: felt, ir0: felt, ir1: felt) {
    return Aqueduct.onFlowUpdate(token, _from, ir0, ir1);
}