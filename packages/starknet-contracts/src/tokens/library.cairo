%lang starknet

from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_le, assert_nn
from starkware.cairo.common.math_cmp import is_not_zero
from starkware.cairo.common.bool import TRUE
from starkware.cairo.common.bitwise import bitwise_not

from openzeppelin.utils.constants.library import UINT8_MAX

from src.utils.SemanticMoney import SemanticMoney, UniversalIndex

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

    func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        recipient: felt, amount: felt
    ) -> (success: felt) {
        let (sender) = get_caller_address();
        _transfer(sender, recipient, amount);
        return (success=TRUE);
    }

    func transfer_from{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        _spend_allowance(sender, caller, amount);
        _transfer(sender, recipient, amount);
        return (success=TRUE);
    }

    func createFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(receiver: felt, flow_rate: felt) {
        let (caller) = get_caller_address();
        with_attr error_message("SuperERC20: balance is zero") {
            let (callerBalance) = balance_of(caller);
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
            let (callerBalance) = balance_of(receiver);
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

    func approve{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, amount: felt
    ) -> (success: felt) {
        with_attr error_message("SuperERC20: amount is negative") {
            assert_nn(amount);
        }
        let (caller) = get_caller_address();
        _approve(caller, spender, amount);
        return (success=TRUE);
    }

    func increase_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, added_value: felt
    ) -> (success: felt) {
        with_attr error("SuperERC20: added_value is negative") {
            assert_nn(added_value);
        }

        let (caller) = get_caller_address();
        let (current_allowance) = SuperERC20_allowances.read(caller, spender);

        // add allowance
        let new_allowance = current_allowance + added_value;
        _approve(caller, spender, new_allowance);
        return (success=TRUE);
    }

    func decrease_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, subtracted_value: felt
    ) -> (success: felt) {
        alloc_locals;
        with_attr error_message("SuperERC20: subtracted_value is negative") {
            assert_nn(subtracted_value);
        }

        let (caller) = get_caller_address();
        let (current_allowance) = SuperERC20_allowances.read(owner=caller, spender=spender);

        let new_allowance = current_allowance - subtracted_value;
        _approve(caller, spender, new_allowance);
        return (success=TRUE);
    }

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
        let (zeroAddressIndex) = SuperERC20_universal_indexes.read(0);
        let (recipientIndex) = SuperERC20_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newZeroAddressIndex, newRecipientIndex) = SemanticMoney.shift2(zeroAddressIndex, recipientIndex, amount, timestamp);
        SuperERC20_universal_indexes.write(0, newZeroAddressIndex);
        SuperERC20_universal_indexes.write(recipient, newRecipientIndex);
        Transfer.emit(0, recipient, amount);
        return ();
    }

    func _transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) {
        with_attr error_message("SuperERC20: amount is not a valid Uint256") {
            assert_nn(amount);  // almost surely not needed, might remove after confirmation
        }

        with_attr error_message("SuperERC20: cannot transfer from the zero address") {
            assert_not_zero(sender);
        }

        with_attr error_message("SuperERC20: cannot transfer to the zero address") {
            assert_not_zero(recipient);
        }
        let (sender_balance) = balance_of(sender);
        with_attr error_message("SuperERC20: transfer amount exceeds balance") {
            let new_sender_balance = sender_balance - amount;
            assert_nn(new_sender_balance);
        }
        let (senderIndex) = SuperERC20_universal_indexes.read(sender);
        let (recipientIndex) = SuperERC20_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(senderIndex, recipientIndex, amount, timestamp);
        SuperERC20_universal_indexes.write(sender, newSenderIndex);
        SuperERC20_universal_indexes.write(recipient, newRecipientIndex);
        Transfer.emit(sender, recipient, amount);
        return ();
    }

    func _spend_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        owner: felt, spender: felt, amount: felt
    ) {
        alloc_locals;
        with_attr error_message("SuperERC20: amount is negative") {
            assert_nn(amount);
        }
        let (current_allowance) = SuperERC20_allowances.read(owner, spender);
        let (infinite) = bitwise_not(0);
        let _is_not_zero = is_not_zero(current_allowance - infinite);

        if (_is_not_zero == TRUE) {
            with_attr error_message("SuperERC20: insufficient allowance") {
                let new_allowance = current_allowance + amount;
                assert_nn(new_allowance);
            }
            _approve(owner, spender, new_allowance);
            return ();
        }
        return ();
    }

    func _approve{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        owner: felt, spender: felt, amount: felt
    ) {
        with_attr error_message("SuperERC20: amount is negative") {
            assert_nn(amount);
        }

        with_attr error_message("SuperERC20: cannot approve from the zero address") {
            assert_not_zero(owner);
        }

        with_attr error_message("SuperERC20: cannot approve to the zero address") {
            assert_not_zero(spender);
        }
        SuperERC20_allowances.write(owner, spender, amount);
        Approval.emit(owner, spender, amount);
        return ();
    }
}
