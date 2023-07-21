%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin

from src.utils.account.library import TestAccount, Call

@external
func execute{syscall_ptr: felt*}(calls_len: felt, calls: Call*, calldata_len:felt, calldata: felt*){
    TestAccount.execute(calls_len, calls, calldata_len, calldata);
    return ();
}