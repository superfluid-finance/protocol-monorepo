%lang starknet

from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_le, assert_nn

from openzeppelin.utils.constants.library import UINT8_MAX

from src.libraries.SemanticMoney import SemanticMoney, UniversalIndex

//
// Events
//

@event
func Transfer(from_: felt, to: felt, value: felt) {
}

@event
func Approval(owner: felt, spender: felt, value: felt) {
}

//
// Storage
//

@storage_var
func SuperERC20_name() -> (name: felt) {
}

@storage_var
func SuperERC20_symbol() -> (symbol: felt) {
}

@storage_var
func SuperERC20_decimals() -> (decimals: felt) {
}

@storage_var
func SuperERC20_total_supply() -> (total_supply: felt) {
}

@storage_var
func SuperERC20_settled_balances(account: felt) -> (balance: felt) {
}

@storage_var
func SuperERC20_allowances(owner: felt, spender: felt) -> (remaining: felt) {
}

@storage_var
func SuperERC20_universal_indexes(address: felt) -> (index: UniversalIndex) {
}

namespace SuperERC20 {
    //
    // Initializer
    //

    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        name: felt, symbol: felt, decimals: felt
    ) {
        SuperERC20_name.write(name);
        SuperERC20_symbol.write(symbol);
        with_attr error_message("SuperERC20: decimals exceed 2^8") {
            assert_le(decimals, UINT8_MAX);
        }
        SuperERC20_decimals.write(decimals);
        return ();
    }

    //
    // Public functions
    //

    func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
        return SuperERC20_name.read();
    }

    func symbol{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        symbol: felt
    ) {
        return SuperERC20_symbol.read();
    }

    func total_supply{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        total_supply: felt
    ) {
        return SuperERC20_total_supply.read();
    }

    func decimals{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        decimals: felt
    ) {
        return SuperERC20_decimals.read();
    }

    func balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (balance: felt) {
        let (_balance) = SuperERC20_settled_balances.read(account);
        let (accountIndex) = SuperERC20_universal_indexes.read(account);
        let (timestamp) = get_block_timestamp();
        let (realtime_balance) = SemanticMoney.realtime_balance_of(accountIndex, timestamp);
        let total_balance = _balance + realtime_balance;
        return (balance = total_balance);
    }

    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (balance: felt) {
        let (accountIndex) = SuperERC20_universal_indexes.read(account);
        let (timestamp) = get_block_timestamp();
        let (realtime_balance) = SemanticMoney.realtime_balance_of(accountIndex, timestamp);
        return (balance = realtime_balance);
    }

    func allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        owner: felt, spender: felt
    ) -> (remaining: felt) {
        return SuperERC20_allowances.read(owner, spender);
    }

    // func settle{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(index: UniversalIndex, account: felt) {
    //     let (timestamp) = get_block_timestamp();
    //     let (newIndex) = SemanticMoney.settle(index);
    //     let (oldBalance) = SuperERC20_settled_balances.read(account);
    //     let newBalance = oldBalance + newIndex.
    //     SuperERC20_settled_balances.write(account, oldBalance);
    //     return ();
    // }

    // func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     recipient: felt, amount: felt
    // ) -> (success: felt) {
    //     let (sender) = get_caller_address();
    //     _transfer(sender, recipient, amount);
    //     return (success=TRUE);
    // }

    // func transfer_from{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     sender: felt, recipient: felt, amount: felt
    // ) -> (success: felt) {
    //     let (caller) = get_caller_address();
    //     _spend_allowance(sender, caller, amount);
    //     _transfer(sender, recipient, amount);
    //     return (success=TRUE);
    // }

    // func approve{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     spender: felt, amount: felt
    // ) -> (success: felt) {
    //     with_attr error_message("SuperERC20: amount is negative") {
    //         assert_nn(amount);
    //     }

    //     let (caller) = get_caller_address();
    //     _approve(caller, spender, amount);
    //     return (success=TRUE);
    // }

    // func increase_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     spender: felt, added_value: felt
    // ) -> (success: felt) {
    //     with_attr error("SuperERC20: added_value is negative") {
    //         assert_nn(added_value);
    //     }

    //     let (caller) = get_caller_address();
    //     let (current_allowance: Uint256) = SuperERC20_allowances.read(caller, spender);

    //     // add allowance
    //     let new_allowance = current_allowance + added_value;
    //     _approve(caller, spender, new_allowance);
    //     return (success=TRUE);
    // }

    // func decrease_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     spender: felt, subtracted_value: felt
    // ) -> (success: felt) {
    //     alloc_locals;
    //     with_attr error_message("SuperERC20: subtracted_value is negative") {
    //         assert_nn(subtracted_value);
    //     }

    //     let (caller) = get_caller_address();
    //     let (current_allowance: Uint256) = SuperERC20_allowances.read(owner=caller, spender=spender);

    //     let new_allowance = current_allowance - subtracted_value;
    //     _approve(caller, spender, new_allowance);
    //     return (success=TRUE);
    // }

    //
    // Internal
    //

    func _mint{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        recipient: felt, amount: felt
    ) {
        with_attr error_message("SuperERC20: amount is negative") {
            assert_nn(amount);
        }

        with_attr error_message("SuperERC20: cannot mint to the zero address") {
            assert_not_zero(recipient);
        }

        let (supply) = SuperERC20_total_supply.read();
        let new_supply = supply + amount;
        SuperERC20_total_supply.write(new_supply);
        let (balance) = SuperERC20_settled_balances.read(account=recipient);
        // overflow is not possible because sum is guaranteed to be less than total supply
        // which we check for overflow below
        let new_balance = balance + amount;
        SuperERC20_settled_balances.write(recipient, new_balance);

        Transfer.emit(0, recipient, amount);
        return ();
    }

    // func _transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
    //     sender: felt, recipient: felt, amount: Uint256
    // ) {
    //     with_attr error_message("SuperERC20: amount is not a valid Uint256") {
    //         assert_nn(amount);  // almost surely not needed, might remove after confirmation
    //     }

    //     with_attr error_message("SuperERC20: cannot transfer from the zero address") {
    //         assert_not_zero(sender);
    //     }

    //     with_attr error_message("SuperERC20: cannot transfer to the zero address") {
    //         assert_not_zero(recipient);
    //     }
    //     let (sender_balance) = SuperERC20_balances.read(sender);
    //     with_attr error_message("SuperERC20: transfer amount exceeds balance") {
    //         let (new_sender_balance) = sender_balance - amount;
    //         assert_nn(new_sender_balance);
    //     }

    //     SuperERC20_balances.write(sender, new_sender_balance);

    //     // add to recipient
    //     let (recipient_balance: Uint256) = SuperERC20_balances.read(account=recipient);
    //     // overflow is not possible because sum is guaranteed by mint to be less than total supply
    //     let (new_recipient_balance) = recipient_balance + amount;
    //     SuperERC20_balances.write(recipient, new_recipient_balance);
    //     Transfer.emit(sender, recipient, amount);
    //     return ();
    // }

    func createFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) {
        let (caller) = get_caller_address();
        with_attr error_message("SuperERC20: balance is zero") {
            let (callerBalance) = SuperERC20_settled_balances.read(receiver);
            assert_not_zero(callerBalance);
        }
        let (callerIndex) = SuperERC20_universal_indexes.read(caller);
        let (receiverIndex) = SuperERC20_universal_indexes.read(receiver);
        let (timestamp) = get_block_timestamp();
        let (newCallerIndex, newReceiverIndex) = SemanticMoney.flow2(callerIndex, receiverIndex, flow_rate, timestamp);
        SuperERC20_universal_indexes.write(caller, newCallerIndex);
        SuperERC20_universal_indexes.write(receiver, newReceiverIndex);
        return ();
    }

    func updateFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) {
        let (caller) = get_caller_address();
        with_attr error_message("SuperERC20: balance is zero") {
            let (callerBalance) = SuperERC20_settled_balances.read(receiver);
            assert_not_zero(callerBalance);
        }
        let (callerIndex) = SuperERC20_universal_indexes.read(caller);
        let (receiverIndex) = SuperERC20_universal_indexes.read(receiver);
        let (timestamp) = get_block_timestamp();
        let (newCallerIndex, newReceiverIndex) = SemanticMoney.flow2(callerIndex, receiverIndex, flow_rate, timestamp);
        SuperERC20_universal_indexes.write(caller, newCallerIndex);
        SuperERC20_universal_indexes.write(receiver, newReceiverIndex);
        return ();
    }
}
