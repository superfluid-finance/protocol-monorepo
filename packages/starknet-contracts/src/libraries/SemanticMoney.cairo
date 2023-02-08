%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

struct UniversalIndex {
    rtb_settled_at: felt,
    rtb_settled_value: felt,
    rtb_flow_rate: felt,
}

// Pure Functions

namespace SemanticMoney {

    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, time: felt) -> (balance: felt) {
        let timeDelta = time - index.rtb_settled_at;
        let balance = (timeDelta * index.rtb_flow_rate) + index.rtb_settled_value;
        return (balance = balance);
    }

    func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, time: felt) -> (index: UniversalIndex) {
        let timeDelta = time - index.rtb_settled_at;
        let newSettledValue = (timeDelta * index.rtb_flow_rate) + index.rtb_settled_value;
        let newUniversalIndex = UniversalIndex(time, newSettledValue, index.rtb_flow_rate);
        return (index = newUniversalIndex);
    }

    func shift1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(amount: felt, index: UniversalIndex) -> (index: UniversalIndex, amount: felt) {
        let newUniversalIndex = UniversalIndex(index.rtb_settled_at, index.rtb_settled_value + amount, index.rtb_flow_rate);
        return (index = newUniversalIndex, amount = amount);
    }

    func shift2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, amount: felt, time: felt) -> (a: UniversalIndex, b: UniversalIndex) {
        let (settledUniversalIndexForA) = settle(a, time);
        let (settledUniversalIndexForB) = settle(b, time);
        // This right-biased. That is: the `amount` argument of shift1 for a is dependent on the `amount` return by shift1 for b (RHS)
        let (bUniversalIndex, _amount) = shift1(amount, settledUniversalIndexForB);
        let (aUniversalIndex, _) = shift1(-_amount, settledUniversalIndexForA);
        return (a = aUniversalIndex, b = bUniversalIndex);
    }

    func setFlow1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(flow_rate: felt, index: UniversalIndex) -> (index: UniversalIndex, flow_rate: felt) {
        let newUniversalIndex = UniversalIndex(index.rtb_settled_at, index.rtb_settled_value, index.rtb_flow_rate + flow_rate);
        return (index = newUniversalIndex, flow_rate = flow_rate);
    }

    func flow2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: UniversalIndex, b: UniversalIndex, flow_rate: felt, time: felt) -> (a: UniversalIndex, b: UniversalIndex) {
        let (settledUniversalIndexForA) = settle(a, time);
        let (settledUniversalIndexForB) = settle(b, time);
        // This is right-biased. That is: the `flow_rate` argument of setFlow1 for a is dependent on the `flow_rate` returned by  setFlow1 for b (RHS)
        let (bUniversalIndex, _flow_rate) = setFlow1(flow_rate, settledUniversalIndexForB);
        let (aUniversalIndex, _) = setFlow1(-_flow_rate, settledUniversalIndexForA);
        return (a = aUniversalIndex, b = bUniversalIndex);
    }
}