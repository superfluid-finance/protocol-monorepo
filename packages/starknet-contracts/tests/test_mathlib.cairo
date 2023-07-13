%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem

from src.utils.MathLib import MathLib


@external
func setup_div{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            value = strategy.integers(-100000000000000000000000, 10000000000000000000000),
            div = strategy.integers(1, 1000000000000000000)
        )
    %}
    return ();
}

@external
func test_div{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(value: felt, div: felt) {
     %{ assume(ids.value > 0) %}
    MathLib.div(value, div);
    return ();
}
