%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

struct UniversalIndex {
    rtb_settled_at: felt,
    rtb_settled_value: felt,
    rtb_flow_rate: felt,
}

namespace SemanticMoney {

    func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, amount: felt, time: felt) -> UniversalIndex {
        let newUniversalIndex = UniversalIndex(time, index.rtb_settled_value + amount, index.rtb_flow_rate);
        return newUniversalIndex;
    }

    func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, amount: felt, time: felt) -> UniversalIndex {
        let aUniversalIndex = shift1(a, -amount, time);
        let bUniversalIndex = shift1(b, amount, time);
        return (aUniversalIndex, bUniversalIndex);
    }

    func flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, flow_rate: felt, time: felt) -> UniversalIndex {
        let newUniversalIndex = UniversalIndex(time, index.rtb_settled_value, index.rtb_flow_rate + flow_rate);
        return newUniversalIndex;
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, flow_rate: felt, time: felt) -> (UniversalIndex, UniversalIndex) {
        let aUniversalIndex = flow1(a, -flow_rate, time);
        let bUniversalIndex = flow1(b, flow_rate, time);
        return (aUniversalIndex, bUniversalIndex);
    }
}