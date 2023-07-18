%lang starknet

from src.utils.account.library import Call

@contract_interface
namespace ITestAccount {
    func execute(calls_len: felt, calls: Call*, calldata_len:felt, calldata: felt*) {
    }
}
