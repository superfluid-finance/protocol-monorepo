%lang starknet

from starkware.starknet.common.syscalls import (
    call_contract
)

struct Call {
    to: felt,
    selector: felt,
}

namespace TestAccount {

    func execute{syscall_ptr: felt*}(calls_len: felt, calls: Call*, calldata_len:felt, calldata: felt*) {
        alloc_locals;
        let this_call: Call = [calls];
        let res = call_contract(
            contract_address=this_call.to,
            function_selector=this_call.selector,
            calldata_size=calldata_len,
            calldata=calldata,
        );
        return ();
    }
}