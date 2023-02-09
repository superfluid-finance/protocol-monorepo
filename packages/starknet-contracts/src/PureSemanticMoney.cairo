%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.libraries.SemanticMoney import SemanticMoney, UniversalIndex

@view
func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, time: felt) -> (balance: felt) {
    return SemanticMoney.realtime_balance_of(index, time);
}

@view
func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, time: felt) -> (index: UniversalIndex) {
    return SemanticMoney.settle(index, time);
}

@view
func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(amount: felt, index: UniversalIndex) -> (index: UniversalIndex, amount: felt) {
    return SemanticMoney.shift1(amount, index);
}

@view
func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, amount: felt, time: felt) -> (a: UniversalIndex, b: UniversalIndex) {
    return SemanticMoney.shift2(a, b, amount, time);
}

@view
func setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(flow_rate: felt, index: UniversalIndex) -> (index: UniversalIndex, flow_rate: felt) {
    return SemanticMoney.setFlow1(flow_rate, index);
}

@view
func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, flow_rate: felt, time: felt) -> (a: UniversalIndex, b: UniversalIndex) {
    return SemanticMoney.flow2(a, b, flow_rate, time);
}
