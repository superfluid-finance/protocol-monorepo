%lang starknet

from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_le, assert_nn
from starkware.cairo.common.math_cmp import is_not_zero
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.bitwise import bitwise_not
from starkware.cairo.common.hash_chain import hash_chain
from starkware.cairo.common.alloc import alloc

from openzeppelin.utils.constants.library import UINT8_MAX

from src.utils.SemanticMoney import SemanticMoney, BasicParticle
from src.interfaces.IPool import IPool

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
func SuperToken_name() -> (name: felt) {
}

@storage_var
func SuperToken_symbol() -> (symbol: felt) {
}

@storage_var
func SuperToken_decimals() -> (decimals: felt) {
}

@storage_var
func SuperToken_allowances(owner: felt, spender: felt) -> (remaining: felt) {
}

@storage_var
func SuperToken_universal_indexes(address: felt) -> (index: BasicParticle) {
}

@storage_var
func SuperToken_flow_rates(address: felt) -> (flow_rate: felt) {
}

@storage_var
func SuperToken_pools(address: felt) -> (bool: felt) {
}

namespace SuperToken {
    //
    // Initializer
    //

    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        name: felt, symbol: felt, decimals: felt
    ) {
        SuperToken_name.write(name);
        SuperToken_symbol.write(symbol);
        with_attr error_message("SuperToken: decimals exceed 2^8") {
            assert_le(decimals, UINT8_MAX);
        }
        SuperToken_decimals.write(decimals);
        return ();
    }

    //
    // Public functions
    //

    func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
        return SuperToken_name.read();
    }

    func symbol{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        symbol: felt
    ) {
        return SuperToken_symbol.read();
    }

    func total_supply{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        total_supply: felt
    ) {
        return (total_supply=0);
    }

    func decimals{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        decimals: felt
    ) {
        return SuperToken_decimals.read();
    }

    func balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (balance: felt) {
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        let (timestamp) = get_block_timestamp();
        let (realtime_balance) = SemanticMoney.realtime_balance_of(accountIndex, timestamp);
        return (balance=realtime_balance);
    }

    func allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        owner: felt, spender: felt
    ) -> (remaining: felt) {
        return SuperToken_allowances.read(owner, spender);
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

    // //////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives starts here
    // //////////////////////////////////////////////////////////////////////////////

    func i_transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        with_attr error("SuperToken: amount is zero") {
            assert_not_zero(amount);
        }
        transfer_from(sender, recipient, amount);
        return (success=TRUE);
    }

    func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, flow_id: felt, flow_rate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert_nn(caller - sender);
            assert_nn(sender - caller);
        }
        let (local data_ptr: felt*) = alloc();
        assert [data_ptr] = sender;
        assert [data_ptr + 1] = recipient;
        assert [data_ptr + 2] = flow_id;
        let (flowAddress) = hash_chain{hash_ptr=pedersen_ptr}(data_ptr);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.flow2(
            senderIndex, recipientIndex, flow_rate, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        SuperToken_flow_rates.write(flowAddress, flow_rate);
        return (success=TRUE);
    }

    func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, poolAddress: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (poolExist) = SuperToken_pools.read(poolAddress);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(poolExist);
        }
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert_nn(caller - sender);
            assert_nn(sender - caller);
        }
        let (distributor) = IPool.distributor(contract_address=poolAddress);
        with_attr error_message("SuperToken: invalid distributor!") {
            assert_nn(sender - distributor);
            assert_nn(distributor - sender);
        }
        let (index) = IPool.getIndex(contract_address=poolAddress);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newPoolIndex, actualAmount) = SemanticMoney.shift2_pd(
            senderIndex, index, reqAmount, timestamp
        );
        IPool.setIndex(contract_address=poolAddress, index=newPoolIndex);
        return (success=TRUE, actualAmount=actualAmount);
    }

    func distribute_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, poolAddress: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt) {
        alloc_locals;
        let (poolExist) = SuperToken_pools.read(poolAddress);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(poolExist);
        }
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert_nn(caller - sender);
            assert_nn(sender - caller);
        }
        let (distributor) = IPool.distributor(contract_address=poolAddress);
        with_attr error_message("SuperToken: invalid distributor!") {
            assert_nn(sender - distributor);
            assert_nn(distributor - sender);
        }
        let (index) = IPool.getIndex(contract_address=poolAddress);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newPoolIndex, actualFlowRate) = SemanticMoney.flow2_pd(
            senderIndex, index, reqFlowRate, timestamp
        );
        IPool.setIndex(contract_address=poolAddress, index=newPoolIndex);
        return (success=TRUE, actualFlowRate=actualFlowRate);
    }

    func connectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt) -> (
        success: felt
    ) {
        return connectPoolEnum(to, TRUE);
    }

    func disconnectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        to: felt
    ) -> (success: felt) {
        return connectPoolEnum(to, FALSE);
    }

    func connectPoolEnum{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        pool: felt, dbConnect: felt
    ) -> (success: felt) {
        // TODO: Add Connection Map
        return (success=TRUE);
    }
    // //////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives ends here
    // //////////////////////////////////////////////////////////////////////////////

    // //////////////////////////////////////////////////////////////////////////////
    // Pool Operations starts here
    // //////////////////////////////////////////////////////////////////////////////

    func createPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        pool: felt
    ) {
        let (caller) = get_caller_address();
        SuperToken_pools.write(caller, TRUE);
        return (pool=caller);
    }

    func absorb{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, particle: BasicParticle
    ) {
        let (caller) = get_caller_address();
        let (poolExist) = SuperToken_pools.read(caller);
        with_attr error_message("SuperToken: Only absorbing from pools!") {
            assert_not_zero(poolExist);
        }
        let (u_index) = SuperToken_universal_indexes.read(account);
        let (new_u_index) = SemanticMoney.mappend(u_index, particle);
        SuperToken_universal_indexes.write(account, new_u_index);
        return ();
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Pool Operations ends here
    // //////////////////////////////////////////////////////////////////////////////

    func approve{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, amount: felt
    ) -> (success: felt) {
        with_attr error_message("SuperToken: amount is negative") {
            assert_nn(amount);
        }
        let (caller) = get_caller_address();
        _approve(caller, spender, amount);
        return (success=TRUE);
    }

    func increase_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, added_value: felt
    ) -> (success: felt) {
        with_attr error("SuperToken: added_value is negative") {
            assert_nn(added_value);
        }

        let (caller) = get_caller_address();
        let (current_allowance) = SuperToken_allowances.read(caller, spender);

        // add allowance
        let new_allowance = current_allowance + added_value;
        _approve(caller, spender, new_allowance);
        return (success=TRUE);
    }

    func decrease_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        spender: felt, subtracted_value: felt
    ) -> (success: felt) {
        alloc_locals;
        with_attr error_message("SuperToken: subtracted_value is negative") {
            assert_nn(subtracted_value);
        }
        let (caller) = get_caller_address();
        let (current_allowance) = SuperToken_allowances.read(owner=caller, spender=spender);

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
        with_attr error_message("SuperToken: amount is negative") {
            assert_nn(amount);
        }

        with_attr error_message("SuperToken: cannot mint to the zero address") {
            assert_not_zero(recipient);
        }
        let (zeroAddressIndex) = SuperToken_universal_indexes.read(0);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newZeroAddressIndex, newRecipientIndex) = SemanticMoney.shift2(
            zeroAddressIndex, recipientIndex, amount, timestamp
        );
        SuperToken_universal_indexes.write(0, newZeroAddressIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        Transfer.emit(0, recipient, amount);
        return ();
    }

    func _transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) {
        with_attr error_message("SuperToken: amount is not a valid Uint256") {
            assert_nn(amount);  // almost surely not needed, might remove after confirmation
        }

        with_attr error_message("SuperToken: cannot transfer from the zero address") {
            assert_not_zero(sender);
        }

        with_attr error_message("SuperToken: cannot transfer to the zero address") {
            assert_not_zero(recipient);
        }
        let (sender_balance) = balance_of(sender);
        with_attr error_message("SuperToken: transfer amount exceeds balance") {
            let new_sender_balance = sender_balance - amount;
            assert_nn(new_sender_balance);
        }
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(
            senderIndex, recipientIndex, amount, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        Transfer.emit(sender, recipient, amount);
        return ();
    }

    func _spend_allowance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        owner: felt, spender: felt, amount: felt
    ) {
        alloc_locals;
        with_attr error_message("SuperToken: amount is negative") {
            assert_nn(amount);
        }
        let (current_allowance) = SuperToken_allowances.read(owner, spender);
        let (infinite) = bitwise_not(0);
        let _is_not_zero = is_not_zero(current_allowance - infinite);

        if (_is_not_zero == TRUE) {
            with_attr error_message("SuperToken: insufficient allowance") {
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
        with_attr error_message("SuperToken: amount is negative") {
            assert_nn(amount);
        }

        with_attr error_message("SuperToken: cannot approve from the zero address") {
            assert_not_zero(owner);
        }

        with_attr error_message("SuperToken: cannot approve to the zero address") {
            assert_not_zero(spender);
        }
        SuperToken_allowances.write(owner, spender, amount);
        Approval.emit(owner, spender, amount);
        return ();
    }
}
