%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem

from src.examples.aqueduct.library import AqueductLibrary, Aqueduct

@external
func setup_cal_new_flowrate_A{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            totalA = strategy.integers(1, 100000), # total A
            totalB = strategy.integers(1, 100000), # total B
            oldFlowRateB = strategy.integers(1, 500), # old flowrate B
            newFlowRateB = strategy.integers(501, 1000), # new flowrate B
        )
    %}
    return ();
}

@external
func test_cal_new_flowrate_A{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    totalA: felt, totalB: felt, oldFlowRateB: felt, newFlowRateB: felt
) {
    let (newFlowRateA) = AqueductLibrary.cal_new_flowrate_A(
        totalA, totalB, oldFlowRateB, newFlowRateB
    );
    let (expected_newFlowRateA, _) = unsigned_div_rem(
        totalA * newFlowRateB, (totalB + newFlowRateB) - oldFlowRateB
    );
    assert newFlowRateA = expected_newFlowRateA;
    return ();
}

@external
func setup_cal_new_flowrate_B{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    %{
        given(
            totalA = strategy.integers(1, 100000), # total A
            totalB = strategy.integers(1, 100000), # total B
            oldFlowRateA = strategy.integers(1, 500), # old flowrate A
            newFlowRateA = strategy.integers(501, 1000), # new flowrate A
        )
    %}
    return ();
}

@external
func test_cal_new_flowrate_B{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    totalA: felt, totalB: felt, oldFlowRateA: felt, newFlowRateA: felt
) {
    let (newFlowRateB) = AqueductLibrary.cal_new_flowrate_B(
        totalA, totalB, oldFlowRateA, newFlowRateA
    );
    let (expected_newFlowRateB, _) = unsigned_div_rem(
        totalB * newFlowRateA, (totalA + newFlowRateA) - oldFlowRateA
    );
    assert newFlowRateB = expected_newFlowRateB;
    return ();
}
