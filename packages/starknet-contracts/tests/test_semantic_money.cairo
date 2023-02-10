%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.utils.SemanticMoney import SemanticMoney, UniversalIndex


@external
func test_realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = UniversalIndex(0, 0, 10);
    let (balance) = SemanticMoney.realtime_balance_of(index, 10);
    assert balance = 100;
    let (_balance) = SemanticMoney.realtime_balance_of(index, 15);
    assert _balance = 150;
    return ();
}

@external
func test_settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = UniversalIndex(0, 0, 10);
    let (newIndex) = SemanticMoney.settle(index, 10);
    assert newIndex.rtb_settled_value = 100;
    assert newIndex.rtb_settled_at = 10;
    let (newIndex) = SemanticMoney.settle(index, 20);
    assert newIndex.rtb_settled_value = 200;
    assert newIndex.rtb_settled_at = 20;
    return ();
}

@external
func test_shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = UniversalIndex(0, 0, 0);
    let (newIndex, _) = SemanticMoney.shift1(10, index);
    assert newIndex.rtb_settled_value = 10;
    let (_newIndex, _) = SemanticMoney.shift1(10, newIndex);
    assert _newIndex.rtb_settled_value = 20;
    let (__newIndex, _) = SemanticMoney.shift1(-10, _newIndex);
    assert __newIndex.rtb_settled_value = 10;
    let (___newIndex, _) = SemanticMoney.shift1(-10, __newIndex);
    assert ___newIndex.rtb_settled_value = 0;
    return ();
}

@external
func test_shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let aIndex = UniversalIndex(0, 0, 0);
    let bIndex = UniversalIndex(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.shift2(aIndex, bIndex, 10, 5);
    assert _aIndex.rtb_settled_value + _bIndex.rtb_settled_value = 0;
    let (__aIndex, __bIndex) = SemanticMoney.shift2(_aIndex, _bIndex, 10, 10);
    assert __aIndex.rtb_settled_value + __bIndex.rtb_settled_value = 0;
    return ();
}

@external
func test_setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let index = UniversalIndex(0, 0, 0);
    let (newIndex, _) = SemanticMoney.setFlow1(10, index);
    assert newIndex.rtb_flow_rate = 10;
    let (newIndex, _) = SemanticMoney.setFlow1(-10, index);
    assert newIndex.rtb_flow_rate = -10;
    return ();
}

@external
func test_flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() {
    let aIndex = UniversalIndex(0, 0, 0);
    let bIndex = UniversalIndex(0, 0, 0);
    let (_aIndex, _bIndex) = SemanticMoney.flow2(aIndex, bIndex, 10, 20);
    assert _aIndex.rtb_flow_rate + _bIndex.rtb_flow_rate = 0;
    let (__aIndex, __bIndex) = SemanticMoney.flow2(_aIndex, _bIndex, 10, 40);
    assert __aIndex.rtb_flow_rate + __bIndex.rtb_flow_rate = 0;
    assert __aIndex.rtb_settled_value = -200;
    assert __bIndex.rtb_settled_value = 200;
    return ();
}
