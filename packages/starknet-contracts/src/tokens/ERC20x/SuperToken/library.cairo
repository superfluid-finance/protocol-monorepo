%lang starknet

from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_le, assert_nn, assert_not_equal
from starkware.cairo.common.math_cmp import is_not_zero, is_le
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.bitwise import bitwise_not
from starkware.cairo.common.hash_chain import hash_chain
from starkware.cairo.common.alloc import alloc
from starkware.starknet.common.syscalls import deploy
from starkware.cairo.common.hash import hash2

from openzeppelin.utils.constants.library import UINT8_MAX

from src.utils.SemanticMoney import SemanticMoney, BasicParticle, PDPoolMemberMU
from src.interfaces.ISuperTokenPool import ISuperTokenPool

const UNSIGNED_FELT_MAX = 2 ** 120;

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
func SuperToken_salt() -> (value: felt) {
}

@storage_var
func SuperToken_pool_class_hash() -> (value: felt) {
}

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
func SuperToken_flow_rates(hash: felt) -> (flow_rate: felt) {
}

@storage_var
func SuperToken_pool_length() -> (value: felt) {
}

@storage_var
func SuperToken_pools(index: felt) -> (pool: felt) {
}

@storage_var
func SuperToken_pool_indexes(pool: felt) -> (index: felt) {
}

@storage_var
func SuperToken_connection_map(account: felt, index: felt) -> (connected: felt) {
}

namespace SuperToken {
    //
    // Initializer
    //

    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        name: felt, symbol: felt, decimals: felt, pool_class_hash: felt
    ) {
        SuperToken_name.write(name);
        SuperToken_symbol.write(symbol);
        with_attr error_message("SuperToken: decimals exceed 2^8") {
            assert_le(decimals, UINT8_MAX);
        }
        SuperToken_decimals.write(decimals);
        SuperToken_pool_class_hash.write(pool_class_hash);
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
        alloc_locals;
        let (timestamp) = get_block_timestamp();
        let (available, _) = realtime_balance_vector_at(account, timestamp);
        let is_less_than_or_equals_zero = is_le(available, 0);
        if (is_less_than_or_equals_zero == TRUE) {
            return (balance=0);
        } else {
            return (balance=available);
        }
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
        _shift(sender, recipient, amount, FALSE);
        return (success=TRUE);
    }

    func transfer_from{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        _shift(sender, recipient, amount, TRUE);
        return (success=TRUE);
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives starts here
    // //////////////////////////////////////////////////////////////////////////////

    func get_net_flow_rate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (flow_rate: felt) {
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        return (flow_rate=accountIndex.flow_rate);
    }

    func get_flow_rate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        _from: felt, to: felt, flow_id: felt
    ) -> (flow_rate: felt) {
        let (hash_from_and_to) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(hash_from_and_to, flow_id);
        return SuperToken_flow_rates.read(flowHash);
    }

    func realtime_balance{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (rtb: felt) {
        let (timestamp) = get_block_timestamp();
        return realtime_balance_at(account, timestamp);
    }

    func realtime_balance_at{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, time: felt
    ) -> (rtb: felt) {
        let (available, deposit) = realtime_balance_vector_at(account, time);
        return (rtb=available - deposit);
    }

    func realtime_balance_vector_at{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(account: felt, time: felt) -> (available: felt, deposit: felt) {
        alloc_locals;
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        let (available) = SemanticMoney.realtime_balance_of(accountIndex, time);
        let (pool_index) = SuperToken_pool_indexes.read(account);
        let is_a_pool = is_not_zero(pool_index);
        if (is_a_pool == TRUE) {
            let (pendingDistribution) = ISuperTokenPool.getPendingDistribution(
                contract_address=account
            );
            let balance = available + pendingDistribution;
            // TODO: Buffer based solvency
            return (available=balance, deposit=0);
        } else {
            let (length) = SuperToken_pool_length.read();
            let (pool_balance) = pool_balance_of(account, length, 0);
            let balance = available + pool_balance;
            // TODO: Buffer based solvency
            return (available=balance, deposit=0);
        }
    }

    func pool_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, pool_length: felt, sum: felt
    ) -> (balance: felt) {
        if (pool_length == 0) {
            return (balance=sum);
        }
        let (pool) = SuperToken_pools.read(pool_length);
        let (connected) = SuperToken_connection_map.read(account, pool_length);
        if (connected == TRUE) {
            let (timestamp) = get_block_timestamp();
            let (balance) = ISuperTokenPool.getClaimable(
                contract_address=pool, time=timestamp, memberAddress=account
            );
            let _sum = sum + balance;
            return pool_balance_of(account, pool_length - 1, _sum);
        } else {
            return pool_balance_of(account, pool_length - 1, sum);
        }
    }

    func shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        let (spender) = get_caller_address();
        if (spender == sender) {
            _shift(sender, recipient, amount, FALSE);
        } else {
            _shift(sender, recipient, amount, TRUE);
        }
        return (success=TRUE);
    }

    func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, flow_id: felt, flow_rate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }

        let (pool_index) = SuperToken_pool_indexes.read(recipient);
        let is_a_pool = is_not_zero(pool_index);
        with_attr error_message("SuperToken: recepient is a pool!") {
            assert_not_equal(is_a_pool, 1);
        }

        let (hash_sender_and_recipient) = hash2{hash_ptr=pedersen_ptr}(sender, recipient);
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(hash_sender_and_recipient, flow_id);

        let (_flowRate) = SuperToken_flow_rates.read(flowHash);
        let flowRateDelta = flow_rate - _flowRate;

        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.shiftFlow2(
            senderIndex, recipientIndex, flowRateDelta, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        SuperToken_flow_rates.write(flowHash, flow_rate);
        return (success=TRUE);
    }

    func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, poolAddress: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (poolIndex) = SuperToken_pool_indexes.read(poolAddress);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(pool);
        }
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }
        let (index) = ISuperTokenPool.getIndex(contract_address=pool);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newPoolIndex, actualAmount) = SemanticMoney.shift2_pd(
            senderIndex, index, reqAmount, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        ISuperTokenPool.operatorSetIndex(contract_address=pool, index=newPoolIndex);
        return (success=TRUE, actualAmount=actualAmount);
    }

    func distribute_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, poolAddress: felt, flow_id: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt) {
        alloc_locals;
        let (poolIndex) = SuperToken_pool_indexes.read(poolAddress);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(pool);
        }

        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }

        let (hash_sender_and_poolAddress) = hash2{hash_ptr=pedersen_ptr}(sender, poolAddress);
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(hash_sender_and_poolAddress, flow_id);
        let (senderFlowRate) = SuperToken_flow_rates.read(flowHash);

        let (index) = ISuperTokenPool.getIndex(contract_address=pool);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let oldFlowRate = -senderIndex.flow_rate;
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newPoolIndex, actualFlowRate) = SemanticMoney.shiftFlow_pd(
            senderIndex, index, reqFlowRate - senderFlowRate, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_flow_rates.write(flowHash, senderFlowRate + actualFlowRate - oldFlowRate);
        ISuperTokenPool.operatorSetIndex(contract_address=pool, index=newPoolIndex);
        return (success=TRUE, actualFlowRate=actualFlowRate);
    }

    func connectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt) -> (
        success: felt
    ) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (connected) = SuperToken_connection_map.read(caller, to);
        with_attr error_message("SuperToken: already connected") {
            assert connected = FALSE;
        }
        return connectPoolEnum(to, TRUE);
    }

    func disconnectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        to: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (connected) = SuperToken_connection_map.read(caller, to);
        with_attr error_message("SuperToken: no connections") {
            assert connected = TRUE;
        }
        return connectPoolEnum(to, FALSE);
    }

    func connectPoolEnum{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        pool: felt, dbConnect: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (poolIndex) = SuperToken_pool_indexes.read(pool);
        let (timestamp) = get_block_timestamp();
        if (dbConnect == TRUE) {
            SuperToken_connection_map.write(caller, poolIndex, TRUE);
            let (connected) = ISuperTokenPool.operatorConnectMember(
                contract_address=pool, memberAddress=caller, dbConnect=TRUE
            );
            assert connected = TRUE;
        } else {
            SuperToken_connection_map.write(caller, poolIndex, FALSE);
            let (disconnected) = ISuperTokenPool.operatorConnectMember(
                contract_address=pool, memberAddress=caller, dbConnect=FALSE
            );
            assert disconnected = TRUE;
        }
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
        alloc_locals;
        let (caller) = get_caller_address();
        let (class_hash) = SuperToken_pool_class_hash.read();
        let (current_salt) = SuperToken_salt.read();
        let (contract_address) = deploy(
            class_hash=class_hash,
            contract_address_salt=current_salt,
            constructor_calldata_size=1,
            constructor_calldata=cast(new (caller,), felt*),
            deploy_from_zero=FALSE,
        );
        SuperToken_salt.write(current_salt + 1);
        let (pool_length) = SuperToken_pool_length.read();
        SuperToken_pool_length.write(pool_length + 1);
        SuperToken_pool_indexes.write(contract_address, pool_length + 1);
        SuperToken_pools.write(pool_length + 1, contract_address);
        _approve(contract_address, contract_address, UNSIGNED_FELT_MAX);
        return (pool=contract_address);
    }

    func isMemberConnected{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        pool: felt, memberAddress: felt
    ) -> (success: felt) {
        let (poolIndex) = SuperToken_pool_indexes.read(pool);
        let (isMemberConnected) = SuperToken_connection_map.read(memberAddress, poolIndex);
        return (success=isMemberConnected);
    }

    func absorbParticleFromPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, particle: BasicParticle
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        let (poolIndex) = SuperToken_pool_indexes.read(caller);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error_message("SuperToken: Only absorbing from pools!") {
            assert_not_zero(pool);
        }
        let (u_index) = SuperToken_universal_indexes.read(account);
        let (new_u_index) = SemanticMoney.mappend(u_index, particle);
        SuperToken_universal_indexes.write(account, new_u_index);
        return (success=TRUE);
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

    func _shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt, checkAllowance: felt
    ) {
        alloc_locals;
        let (pool_index) = SuperToken_pool_indexes.read(recipient);
        let is_a_pool = is_not_zero(pool_index);
        with_attr error_message("SuperToken: recepient is a pool!") {
            assert_not_equal(is_a_pool, 1);
        }
        with_attr error_message("SuperToken: amount is negative!") {
            assert_nn(amount);
        }
        let (spender) = get_caller_address();
        if (checkAllowance == TRUE) {
            _spend_allowance(sender, spender, amount);
            let (senderIndex) = SuperToken_universal_indexes.read(sender);
            let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
            let (timestamp) = get_block_timestamp();
            let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(
                senderIndex, recipientIndex, amount, timestamp
            );
            SuperToken_universal_indexes.write(sender, newSenderIndex);
            SuperToken_universal_indexes.write(recipient, newRecipientIndex);
            return ();
        } else {
            let (senderIndex) = SuperToken_universal_indexes.read(sender);
            let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
            let (timestamp) = get_block_timestamp();
            let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(
                senderIndex, recipientIndex, amount, timestamp
            );
            SuperToken_universal_indexes.write(sender, newSenderIndex);
            SuperToken_universal_indexes.write(recipient, newRecipientIndex);
            return ();
        }
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
