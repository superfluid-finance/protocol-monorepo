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


//
// Structs
//

struct AccountData {
    totalBuffer: felt,
    totalInflowRate: felt,
    totalOutflowRate: felt,
}

struct FlowData {
    _from: felt,
    to: felt,
    flowRate: felt,
    buffer: felt,
}


//
// Events
//

@event
func Transfer(from_: felt, to: felt, value: felt) {
}

@event
func Approval(owner: felt, spender: felt, value: felt) {
}

@event
func PoolCreated(admin: felt, poolAddress: felt) {
}

@event
func FlowCreated(sender: felt, recipient: felt, flowId: felt, flowRate: felt) {
}

@event
func FlowDistributed(sender: felt, poolAddress: felt, flowId: felt, flowRate: felt) {
}

@event
func Distributed(sender: felt, poolAddress: felt, amount: felt) {
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
func SuperToken_universal_indexes(address: felt) -> (index: BasicParticle) {
}

@storage_var
func SuperToken_flow_data(hash: felt) -> (data: FlowData) {
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

@storage_var
func SuperToken_account_data(account: felt) -> (data: AccountData) {
}

@storage_var
func SuperToken_liquidation_period() -> (value: felt) {
}

@storage_var
func SuperToken_connected_pool_length(account: felt) -> (value: felt) {
}

const DISTRIBUTE_FLOW_AS_FELT = 133470332451617709077403932975189225335;

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
        SuperToken_liquidation_period.write(42 * 60);
        return ();
    }

    func setLiquadationPeriod{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(value: felt) {
        with_attr error_message("SuperToken: liquidation value is negative") {
            assert_nn(value);
        }
        SuperToken_liquidation_period.write(value);
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

    // // //////////////////////////////////////////////////////////////////////////////
    // // Generalized Payment Primitives starts here
    // // //////////////////////////////////////////////////////////////////////////////


    func realtime_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (rtb: felt) {
        let (timestamp) = get_block_timestamp();
        return realtime_balance_at(account, timestamp);
    }

    func realtime_balance_at{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, time: felt
    ) -> (rtb: felt) {
        let (available, _) = realtime_balance_vector_at(account, time);
        return (rtb=available);
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
            let (length) = SuperToken_pool_length.read();
            let (pool_balance) = pool_balance_of(account, length, 0);
            let total_balance = balance + pool_balance;
            let (accountData) = SuperToken_account_data.read(account);
            let deposit = accountData.totalBuffer;
            return (available=total_balance, deposit=deposit);
        } else {
            let (length) = SuperToken_pool_length.read();
            let (pool_balance) = pool_balance_of(account, length, 0);
            let balance = available + pool_balance;
            let (accountData) = SuperToken_account_data.read(account);
            let deposit = accountData.totalBuffer;
            return (available=balance, deposit=deposit);
        }
    }

    func pool_balance_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, pool_length: felt, sum: felt
    ) -> (balance: felt) {
        if (pool_length == 0) {
            return (balance=sum);
        }
        let (connected) = SuperToken_connection_map.read(account, pool_length);
        if (connected == TRUE) {
            let (pool) = SuperToken_pools.read(pool_length);
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

    func get_flow_hash{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, to: felt, flow_id) -> (hash: felt) {
        let (hash_sender_and_recipient) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(hash_sender_and_recipient, flow_id);
        return (hash=flowHash);
    }

    func get_distribution_flow_hash{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(_from: felt, to: felt, flow_id) -> (hash: felt) {
        let (hash_sender_and_poolAddress) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (hash_sender_and_poolAddress_and_flow_type) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_poolAddress, DISTRIBUTE_FLOW_AS_FELT
        );
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_poolAddress_and_flow_type, flow_id
        );
        return (hash=flowHash);
    }

    func get_net_flow_rate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (flow_rate: felt) {
        alloc_locals;
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        let flow_rate = accountIndex._flow_rate;
        let (pool_index) = SuperToken_pool_indexes.read(account);
        let is_a_pool = is_not_zero(pool_index);

        if (is_a_pool == TRUE) {
            let (pendingDistributionFlowRate) = ISuperTokenPool.getPendingDistributionFlowRate(
                contract_address=account
            );
            let nr = flow_rate + pendingDistributionFlowRate;
            let (length) = SuperToken_pool_length.read();
            let (pool_flow_rate) = pool_flow_rate_of(account, length, 0);
            let total_flow_rate = nr + pool_flow_rate;
            return (flow_rate=total_flow_rate);
        }

        let (length) = SuperToken_pool_length.read();
        let (pool_flow_rate) = pool_flow_rate_of(account, length, 0);
        let nr = flow_rate + pool_flow_rate;
        return (flow_rate=nr);
    }

    func pool_flow_rate_of{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, pool_length: felt, sum: felt
    ) -> (flow_rate: felt) {
        if (pool_length == 0) {
            return (flow_rate=sum);
        }
        let (connected) = SuperToken_connection_map.read(account, pool_length);
        if (connected == TRUE) {
            let (pool) = SuperToken_pools.read(pool_length);
            let (flow_rate) = ISuperTokenPool.getMemberFlowRate(
                contract_address=pool, memberAddress=account
            );
            let _sum = sum + flow_rate;
            return pool_flow_rate_of(account, pool_length - 1, _sum);
        } else {
            return pool_flow_rate_of(account, pool_length - 1, sum);
        }
    }

    func get_flow_rate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        _from: felt, to: felt, flow_id: felt
    ) -> (flow_rate: felt) {
        let (flowHash) = get_flow_hash(_from, to, flow_id);
        let (flowData) = SuperToken_flow_data.read(flowHash);
        return (flow_rate=flowData.flowRate);
    }

    func shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        _shift(caller, sender, recipient, amount);
        return (success=TRUE);
    }

    func _shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(operator: felt, sender: felt, recipient: felt, amount: felt) {
        alloc_locals;
        let (pool_index) = SuperToken_pool_indexes.read(recipient);
        let is_a_pool = is_not_zero(pool_index);
        with_attr error_message("SuperToken: recepient is a pool!") {
            assert_not_equal(is_a_pool, 1);
        }
        with_attr error_message("SuperToken: amount is negative!") {
            assert_nn(amount);
        }
        let (aclStatus) = _acl(operator, sender);
        with_attr error_message("SuperToken: ACL for shift not supported") {
            assert aclStatus = TRUE;
        }
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(
            senderIndex, recipientIndex, amount
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        return ();
    }

    func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, flow_id: felt, flow_rate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _flow(caller, sender, recipient, flow_id, flow_rate);
    }

    func _flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, recipient: felt, flow_id: felt, flow_rate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }
        with_attr error_message("SuperToken: negative flow rate not allowed") {
            assert_nn(flow_rate);
        }

        let (pool_index) = SuperToken_pool_indexes.read(recipient);
        let is_a_pool = is_not_zero(pool_index);
        with_attr error_message("SuperToken: recepient is a pool!") {
            assert_not_equal(is_a_pool, 1);
        }

        let (aclStatus) = _acl(operator, sender);
        with_attr error_message("SuperToken: ACL for flow not supported") {
            assert aclStatus = TRUE;
        }

        let (flowHash) = get_flow_hash(sender, recipient, flow_id);

        let (flowData) = SuperToken_flow_data.read(flowHash);
        let _flowRate = flowData.flowRate;
        let flowRateDelta = flow_rate - _flowRate;

        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (recipientIndex) = SuperToken_universal_indexes.read(recipient);
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newRecipientIndex) = SemanticMoney.shiftFlow2(
            senderIndex, recipientIndex, flowRateDelta, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        _setFlowInfo(flowHash, sender, recipient, flow_rate, flowRateDelta);
        _adjustBuffer(sender, flowHash, flow_rate);
        FlowCreated.emit(sender, recipient, flow_id, flow_rate);
        return (success=TRUE);
    }

    func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(sender: felt, poolAddress: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _distribute(caller, sender, poolAddress, reqAmount);
    }

    func _distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, poolAddress: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (poolIndex) = SuperToken_pool_indexes.read(poolAddress);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(pool);
        }
        with_attr error_message("SuperToken: negative amount not allowed") {
            assert_nn(reqAmount);
        }
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }

        let (aclStatus) = _acl(operator, sender);
        with_attr error_message("SuperToken: ACL for flow not supported") {
            assert aclStatus = TRUE;
        }

        let (index) = ISuperTokenPool.getIndex(contract_address=pool);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let (newSenderIndex, newPoolIndex, actualAmount) = SemanticMoney.shift2_pd(
            senderIndex, index, reqAmount
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        ISuperTokenPool.operatorSetIndex(contract_address=pool, index=newPoolIndex);
        Distributed.emit(sender, poolAddress, actualAmount);
        return (success=TRUE, actualAmount=actualAmount);
    }

    func distribute_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, poolAddress: felt, flow_id: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _distribute_flow(caller, sender, poolAddress, flow_id, reqFlowRate);
    }

    func _distribute_flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, poolAddress: felt, flow_id: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt) {
        alloc_locals;
        let (poolIndex) = SuperToken_pool_indexes.read(poolAddress);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error("SuperToken: Pool does not exist") {
            assert_not_zero(pool);
        }
        with_attr error_message("SuperToken: negative flow_rate not allowed") {
            assert_nn(reqFlowRate);
        }
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }

        let (aclStatus) = _acl(operator, sender);
        with_attr error_message("SuperToken: ACL for flow not supported") {
            assert aclStatus = TRUE;
        }

        let (flowHash) = get_distribution_flow_hash(sender, poolAddress, flow_id);

        let (flowData) = SuperToken_flow_data.read(flowHash);
        let flowRate = flowData.flowRate;

        let (index) = ISuperTokenPool.getIndex(contract_address=pool);
        let (senderIndex) = SuperToken_universal_indexes.read(sender);
        let oldFlowRate = -senderIndex._flow_rate;
        let (timestamp) = get_block_timestamp();
        let (newSenderIndex, newPoolIndex, actualFlowRate) = SemanticMoney.shiftFlow_pd(
            senderIndex, index, reqFlowRate - flowRate, timestamp
        );
        SuperToken_universal_indexes.write(sender, newSenderIndex);
        ISuperTokenPool.operatorSetIndex(contract_address=pool, index=newPoolIndex);
        _setFlowInfo(flowHash, sender, poolAddress, actualFlowRate, actualFlowRate - flowRate);
        _adjustBuffer(sender, flowHash, actualFlowRate);
        FlowDistributed.emit(sender, poolAddress, flow_id, actualFlowRate);
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
            let (connectedPoolLength) = SuperToken_connected_pool_length.read(caller);
            SuperToken_connected_pool_length.write(caller, connectedPoolLength + 1);
        } else {
            SuperToken_connection_map.write(caller, poolIndex, FALSE);
            let (disconnected) = ISuperTokenPool.operatorConnectMember(
                contract_address=pool, memberAddress=caller, dbConnect=FALSE
            );
            assert disconnected = TRUE;
            let (connectedPoolLength) = SuperToken_connected_pool_length.read(caller);
            SuperToken_connected_pool_length.write(caller, connectedPoolLength - 1);
        }
        return (success=TRUE);
    }
    // // //////////////////////////////////////////////////////////////////////////////
    // // Generalized Payment Primitives ends here
    // // //////////////////////////////////////////////////////////////////////////////

    // // //////////////////////////////////////////////////////////////////////////////
    // ///////// Pool Operations starts here
    // // //////////////////////////////////////////////////////////////////////////////

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
        PoolCreated.emit(caller, contract_address);
        return (pool=contract_address);
    }

    func isMemberConnected{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        pool: felt, memberAddress: felt
    ) -> (success: felt) {
        let (poolIndex) = SuperToken_pool_indexes.read(pool);
        let (isMemberConnected) = SuperToken_connection_map.read(memberAddress, poolIndex);
        return (success=isMemberConnected);
    }

    func getNumConnections{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (value: felt) {
        return SuperToken_connected_pool_length.read(account);
    }

    func absorbParticleFromPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        accounts_len: felt, accounts: felt*, particles_len: felt, particles: BasicParticle*
    ) -> (success: felt) {
        with_attr error_message("SuperToken: account len != particle length") {
            assert accounts_len = particles_len;
        }
        if (accounts_len == 0) {
            return (success=TRUE);
        }
        let (caller) = get_caller_address();
        let (poolIndex) = SuperToken_pool_indexes.read(caller);
        let (pool) = SuperToken_pools.read(poolIndex);
        with_attr error_message("SuperToken: Only absorbing from pools!") {
            assert_not_zero(pool);
        }
        let (u_index) = SuperToken_universal_indexes.read([accounts]);
        let (new_u_index) = SemanticMoney.mappend(u_index, [particles]);
        SuperToken_universal_indexes.write([accounts], new_u_index);
        return absorbParticleFromPool(
            accounts_len - 1, accounts + 1, particles_len - 1, particles + 1
        );
    }

    ///////////////////////////////////////////////////////////////////////////////////
    //////////// Pool Operations ends here
    ///////////////////////////////////////////////////////////////////////////////////
    
    ////////////////////////////////////////////////////////////////////////////////////
    /////////// Buffer Solvency
    ////////////////////////////////////////////////////////////////////////////////////
    func _adjustBuffer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, flow_hash: felt, newflowRate: felt){
        let (lp) = SuperToken_liquidation_period.read();
        let newBufferAmount = newflowRate * lp;
        let (flowData) = SuperToken_flow_data.read(flow_hash);
        let bufferDelta = newBufferAmount - flowData.buffer;
        let (senderIndex) = SuperToken_universal_indexes.read(_from);
        let (contract_address) = get_contract_address();
        let (recipientIndex) = SuperToken_universal_indexes.read(contract_address);
        let (_senderIndex, _recipientIndex) = SemanticMoney.shift2(senderIndex, recipientIndex, bufferDelta);
        SuperToken_universal_indexes.write(_from, _senderIndex);
        SuperToken_universal_indexes.write(contract_address, _recipientIndex);

        let (accountData) = SuperToken_account_data.read(_from);
        let newAccountData = AccountData(accountData.totalBuffer + bufferDelta, accountData.totalInflowRate, accountData.totalOutflowRate);
        SuperToken_account_data.write(_from, newAccountData);
        
        let newFlowData = FlowData(flowData._from, flowData.to, flowData.flowRate, newBufferAmount);
        SuperToken_flow_data.write(flow_hash, newFlowData);
        return ();
    }

    ////////////////////////////////////////////////////////////////////////////////////
    //// Others
    ////////////////////////////////////////////////////////////////////////////////////

    func _setFlowInfo{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(flow_hash: felt, _from: felt, to: felt, newflowRate: felt, flowRateDelta: felt){
        let (flowData) = SuperToken_flow_data.read(flow_hash);
        let newFlowData = FlowData(flowData._from, flowData.to, newflowRate, flowData.buffer);
        SuperToken_flow_data.write(flow_hash, newFlowData);

        let (senderAccountData) = SuperToken_account_data.read(_from);
        let newSenderAccountData = AccountData(senderAccountData.totalBuffer, senderAccountData.totalInflowRate, senderAccountData.totalOutflowRate + flowRateDelta);
        SuperToken_account_data.write(_from, newSenderAccountData);

        let (recipientAccountData) = SuperToken_account_data.read(to);
        let newRecipienAccountData = AccountData(recipientAccountData.totalBuffer, recipientAccountData.totalInflowRate + flowRateDelta, recipientAccountData.totalOutflowRate);
        SuperToken_account_data.write(to, recipientAccountData);
        return ();
    }

    func _acl{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(operator: felt, _from: felt) -> (status: felt){
        if (operator == _from){
            return (status=TRUE);
        } else {
            return (status=FALSE);
        }
    }

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
        let (newZeroAddressIndex, newRecipientIndex) = SemanticMoney.shift2(
            zeroAddressIndex, recipientIndex, amount
        );
        SuperToken_universal_indexes.write(0, newZeroAddressIndex);
        SuperToken_universal_indexes.write(recipient, newRecipientIndex);
        Transfer.emit(0, recipient, amount);
        return ();
    }
}
