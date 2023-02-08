%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

struct UniversalIndex {
    rtb_settled_at: felt,
    rtb_settled_value: felt,
    rtb_flow_rate: felt,
}

namespace SemanticMoney {

    func flow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, flow_rate: felt) -> UniversalIndex {
        let newUniversalIndex = UniversalIndex(index.rtb_settled_at, index.rtb_settled_value, index.rtb_flow_rate + flow_rate);
        return newUniversalIndex;
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, flow_rate: felt) -> (UniversalIndex, UniversalIndex) {
        let aUniversalIndex = flow1(a, -flow_rate);
        let bUniversalIndex = flow1(b, flow_rate);
        return (aUniversalIndex, bUniversalIndex);
    }
}