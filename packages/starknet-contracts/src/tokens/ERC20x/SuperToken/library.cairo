%lang starknet

from starkware.starknet.common.syscalls import (
    get_caller_address,
    get_block_timestamp,
    get_contract_address,
)
from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import assert_not_zero, assert_le, assert_nn, assert_not_equal
from starkware.cairo.common.math_cmp import is_not_zero, is_le, is_nn
from starkware.cairo.common.bool import TRUE, FALSE
from starkware.cairo.common.bitwise import bitwise_not
from starkware.cairo.common.hash_chain import hash_chain
from starkware.cairo.common.alloc import alloc
from starkware.starknet.common.syscalls import deploy
from starkware.cairo.common.hash import hash2

from openzeppelin.utils.constants.library import UINT8_MAX

from src.utils.SemanticMoney import SemanticMoney, BasicParticle, PDPoolMemberMU
from src.interfaces.ISuperfluidPool import ISuperfluidPool


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

const FLOW_AS_FELT = 1718382455;
const DISTRIBUTE_FLOW_AS_FELT = 133470332451617709077403932975189225335;
const POOL_ADJUSTMENT_FLOW_AS_FELT = 641892055390323327801894208654700561001886805879;

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
        SuperToken_liquidation_period.write(1000);
        return ();
    }

    func name{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (name: felt) {
        return SuperToken_name.read();
    }

    func symbol{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        symbol: felt
    ) {
        return SuperToken_symbol.read();
    }

    func totalSupply{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        totalSupply: felt
    ) {
        return (totalSupply=0);
    }

    func decimals{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        decimals: felt
    ) {
        return SuperToken_decimals.read();
    }

    func balanceOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (balance: felt) {
        alloc_locals;
        let (timestamp) = get_block_timestamp();
        let (rtb) = realtimeBalanceAt(account, timestamp);
        let is_less_than_or_equals_zero = is_le(rtb, 0);
        if (is_less_than_or_equals_zero == TRUE) {
            return (balance=0);
        } else {
            return (balance=rtb);
        }
    }

    func transfer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt, amount: felt) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _shift(caller, caller, to, amount);
    }

    func transferFrom{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, to: felt, amount: felt) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _shift(caller, _from, to, amount);
    }

    // // //////////////////////////////////////////////////////////////////////////////
    // // Generalized Payment Primitives starts here
    // // //////////////////////////////////////////////////////////////////////////////

    func realtimeBalanceAt{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, time: felt
    ) -> (rtb: felt) {
        let (own, fromPool ,_) = realtimeBalanceVectorAt(account, time);
        return (rtb = own + fromPool);
    }

    func realtimeBalanceVectorAt{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(account: felt, time: felt) -> (own: felt, fromPool: felt, deposit: felt) {
        alloc_locals;
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        let (available) = SemanticMoney.realtime_balance_of(accountIndex, time);
        let (isAPool) = isPool(account);
        if (isAPool == TRUE) {
            let (own) = ISuperfluidPool.getDisconnectedBalance(contract_address=account, time=time);
            let (length) = SuperToken_pool_length.read();
            let (poolBalance) = poolBalanceOf(account, length, 0);
            let (accountData) = SuperToken_account_data.read(account);
            let deposit = accountData.totalBuffer;
            return (own=own, fromPool=poolBalance, deposit=deposit);
        } else {
            let (uIndex) = SuperToken_universal_indexes.read(account);
            let (own) = SemanticMoney.realtime_balance_of(uIndex, time);
            let (length) = SuperToken_pool_length.read();
            let (poolBalance) = poolBalanceOf(account, length, 0);
            let (accountData) = SuperToken_account_data.read(account);
            let deposit = accountData.totalBuffer;
            return (own=own, fromPool=poolBalance, deposit=deposit);
        }
    }

    func poolBalanceOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, pool_length: felt, sum: felt
    ) -> (balance: felt) {
        if (pool_length == 0) {
            return (balance=sum);
        }
        let (connected) = SuperToken_connection_map.read(account, pool_length);
        if (connected == TRUE) {
            let (pool) = SuperToken_pools.read(pool_length);
            let (timestamp) = get_block_timestamp();
            let (balance) = ISuperfluidPool.getClaimable(
                contract_address=pool, time=timestamp, memberAddress=account
            );
            let _sum = sum + balance;
            return poolBalanceOf(account, pool_length - 1, _sum);
        } else {
            return poolBalanceOf(account, pool_length - 1, sum);
        }
    }

    func getNetFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (flowRate: felt) {
        alloc_locals;
        let (accountIndex) = SuperToken_universal_indexes.read(account);
        let (flowRate) = SemanticMoney.flow_rate(accountIndex);
        let (isAPool) = isPool(account);

        if (isAPool == TRUE) {
            let (dfr) = ISuperfluidPool.getDisconnectedFlowRate(contract_address=account);
            let nr = flowRate + dfr;
            let (length) = SuperToken_pool_length.read();
            let (poolFlowRate) = poolFlowRateOf(account, length, 0);
            let totalFlowRate = nr + poolFlowRate;
            return (flowRate=totalFlowRate);
        } else {
            let (length) = SuperToken_pool_length.read();
            let (poolFlowRate) = poolFlowRateOf(account, length, 0);
            let nr = flowRate + poolFlowRate;
            return (flowRate=nr);
        }
    }

    func poolFlowRateOf{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt, pool_length: felt, sum: felt
    ) -> (flow_rate: felt) {
        if (pool_length == 0) {
            return (flow_rate=sum);
        }
        let (connected) = SuperToken_connection_map.read(account, pool_length);
        if (connected == TRUE) {
            let (pool) = SuperToken_pools.read(pool_length);
            let (flow_rate) = ISuperfluidPool.getMemberFlowRate(
                contract_address=pool, memberAddress=account
            );
            let _sum = sum + flow_rate;
            return poolFlowRateOf(account, pool_length - 1, _sum);
        } else {
            return poolFlowRateOf(account, pool_length - 1, sum);
        }
    }

    func getFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        _from: felt, to: felt, flowId: felt
    ) -> (flowRate: felt) {
        let (flowHash) = getFlowHash(_from, to, flowId);
        let (flowRate) = _getFlowRate(flowHash);
        return (flowRate=flowRate);
    }

    func getFlowHash{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, to: felt, flow_id: felt) -> (hash: felt) {
        let (hash_sender_and_recipient) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (hash_sender_and_recipient_and_flow_type) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_recipient, FLOW_AS_FELT
        );
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(hash_sender_and_recipient_and_flow_type, flow_id);
        return (hash=flowHash);
    }

    func getDistributionFlowHash{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(_from: felt, to: felt, flowId: felt) -> (hash: felt) {
        let (hash_sender_and_poolAddress) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (hash_sender_and_poolAddress_and_flow_type) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_poolAddress, DISTRIBUTE_FLOW_AS_FELT
        );
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_poolAddress_and_flow_type, flowId
        );
        return (hash=flowHash);
    }

    func getPoolAdjustmentFlowHash{
        syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr
    }(_from: felt, to: felt) -> (hash: felt) {
        let (hash_sender_and_poolAddress) = hash2{hash_ptr=pedersen_ptr}(_from, to);
        let (flowHash) = hash2{hash_ptr=pedersen_ptr}(
            hash_sender_and_poolAddress, POOL_ADJUSTMENT_FLOW_AS_FELT
        );
        return (hash=flowHash);
    }

    func shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, amount: felt
    ) -> (success: felt) {
        let (caller) = get_caller_address();
        return _shift(caller, sender, recipient, amount);
    }

    func _shift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(operator: felt, sender: felt, recipient: felt, amount: felt)  -> (success: felt) {
        alloc_locals;
        let (is_a_pool) = isPool(recipient);
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
        _doShift(sender, recipient, amount);
        return (success=TRUE);
    }

    func _doShift{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, to: felt, amount: felt) -> (success: felt) {
        alloc_locals;
        if (_from == to) {
            return (success=TRUE);
        } else {
            let (senderIndex) = SuperToken_universal_indexes.read(_from);
            let (recipientIndex) = SuperToken_universal_indexes.read(to);
            let (newSenderIndex, newRecipientIndex) = SemanticMoney.shift2(
                senderIndex, recipientIndex, amount
            );
            SuperToken_universal_indexes.write(_from, newSenderIndex);
            SuperToken_universal_indexes.write(to, newRecipientIndex);
            return (success=TRUE);
        }
    }


    func flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, recipient: felt, flowId: felt, flowRate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _flow(caller, sender, recipient, flowId, flowRate);
    }

    func _flow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, recipient: felt, flowId: felt, flowRate: felt
    ) -> (success: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        with_attr error_message("SuperToken: invalid sender!") {
            assert caller = sender;
        }
        with_attr error_message("SuperToken: negative flow rate not allowed") {
            assert_nn(flowRate);
        }

        let (is_a_pool) = isPool(recipient);
        with_attr error_message("SuperToken: recepient is a pool!") {
            assert_not_equal(is_a_pool, 1);
        }

        let (aclStatus) = _acl(operator, sender);
        with_attr error_message("SuperToken: ACL for flow not supported") {
            assert aclStatus = TRUE;
        }

        let (flowHash) = getFlowHash(sender, recipient, flowId);
        let (time) = get_block_timestamp(); 
        _doFlow(sender, recipient, flowHash, flowRate, time);
        _adjustBuffer(sender, flowHash, flowRate);
        FlowCreated.emit(sender, recipient, flowId, flowRate);
        return (success=TRUE);
    }

    func _doFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        _from: felt, to: felt, flowHash: felt, flowRate: felt, time: felt
    ) -> (success: felt) {
        alloc_locals;
        if (_from == to) {
            return (success=TRUE);
        } else {
            let (oldFlowRate) = _getFlowRate(flowHash);
            let flowRateDelta = flowRate - oldFlowRate;
            let (uIndexForFrom) = SuperToken_universal_indexes.read(_from);
            let (uIndexForTo) = SuperToken_universal_indexes.read(to);
            let (newUIndexForFrom, newUIndexForTo) = SemanticMoney.shiftFlow2(uIndexForFrom, uIndexForTo, flowRateDelta, time);
            SuperToken_universal_indexes.write(_from, newUIndexForFrom);
            SuperToken_universal_indexes.write(to, newUIndexForTo);
            _setFlowInfo(flowHash, _from, to, flowRate, flowRateDelta);
            return (success=TRUE);
        }
    }

    func distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(sender: felt, poolAddress: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _distribute(caller, sender, poolAddress, reqAmount);
    }

    func _distribute{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, pool: felt, reqAmount: felt
    ) -> (success: felt, actualAmount: felt) {
        alloc_locals;
        let (is_a_pool) = isPool(pool);
        with_attr error("SuperToken: Pool does not exist") {
            assert is_a_pool = TRUE;
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

        let (actualAmount) = _doDistributeViaPool(sender, pool, reqAmount);
        Distributed.emit(sender, pool, actualAmount);
        return (success=TRUE, actualAmount=actualAmount);
    }

    func _doDistributeViaPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, pool: felt, reqAmount: felt) -> (actualAmount: felt) {
        alloc_locals;
        with_attr error_message("SuperToken: sender can't be equal to pool") {
            assert_not_equal(_from, pool);
        }
        let (uIndexForFrom) = SuperToken_universal_indexes.read(_from);
        let (pdIndex) = ISuperfluidPool.getIndex(contract_address=pool);
        let (newUIndexForFrom, newPdIndex, actualAmount) = SemanticMoney.shift2_pd(
            uIndexForFrom, pdIndex, reqAmount
        );
        SuperToken_universal_indexes.write(_from, newUIndexForFrom);
        ISuperfluidPool.operatorSetIndex(contract_address=pool, index=newPdIndex);
        return (actualAmount=actualAmount);
    }

    func distributeFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        sender: felt, pool: felt, flowId: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt, newDistributionFlowRate: felt) {
        alloc_locals;
        let (caller) = get_caller_address();
        return _distributeFlow(caller, sender, pool, flowId, reqFlowRate);
    }

    func _distributeFlow{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        operator: felt, sender: felt, pool: felt, flowId: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt, newDistributionFlowRate: felt) {
        alloc_locals;
        let (is_a_pool) = isPool(pool);
        with_attr error("SuperToken: Pool does not exist") {
            assert is_a_pool = TRUE;
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

        let (flowHash) = getDistributionFlowHash(sender, pool, flowId);
        let (time) = get_block_timestamp();
        let (newActualFlowRate, newDistributionFlowRate) = _doDistributeFlowViaPool(sender, pool, flowHash, reqFlowRate, time);
        _adjustBuffer(sender, flowHash, newActualFlowRate);
        FlowDistributed.emit(sender, pool, flowId, newActualFlowRate);
        return (success=TRUE, actualFlowRate=newActualFlowRate, newDistributionFlowRate=newDistributionFlowRate);
    }

    func _doDistributeFlowViaPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, pool: felt, flowHash: felt, reqFlowRate: felt, time: felt) -> (newActualFlowRate: felt, newDistributionFlowRate: felt) {
        alloc_locals;
        with_attr error_message("SuperToken: sender can't be equal to pool") {
            assert_not_equal(_from, pool);
        }
        // uIndexForFrom: from uidx -> uIndexForPool: pool uidx -> pdIndex: pool pdpidx
        // uIndexForPool handles the adjustment flow through _get/_setPoolAdjustmentFlowRate.
        let (uIndexForFrom) = SuperToken_universal_indexes.read(_from);
        let (uIndexForPool) = SuperToken_universal_indexes.read(pool);
        let (pdIndex) = ISuperfluidPool.getIndex(contract_address=pool);
        let (currentAdjustmentFlowRate) = _getPoolAdjustmentFlowRate(pool);

        let (oldFlowRate) = _getFlowRate(flowHash); // flow rate of : from -> pool
        let (oldDistributionFlowRate) = SemanticMoney.flow_rate_for_pool_index(pdIndex);
        let shiftFlowRate = reqFlowRate - oldFlowRate;


        // to readjust, include the current adjustment flow rate here
        let (newUIndexForPool, newPdIndex, newDistributionFlowRate) = SemanticMoney.shiftFlow_pd(uIndexForPool, pdIndex, shiftFlowRate + currentAdjustmentFlowRate, time);

        with_attr error_message("SuperToken: distribution flow rate cannot be negative") {
            assert_nn(newDistributionFlowRate);
        }
        let newActualFlowRate = oldFlowRate + (newDistributionFlowRate - oldDistributionFlowRate) - currentAdjustmentFlowRate;
        let newActualFlowRateisNotNeg = is_nn(newActualFlowRate);
        if (newActualFlowRateisNotNeg == TRUE){
            let newAdjustmentFlowRate = 0;
            let actualFlowRateDelta = newActualFlowRate - oldFlowRate;
            let (newUIndexForFrom, _newUIndexForPool) = SemanticMoney.shiftFlow2(uIndexForFrom, newUIndexForPool, actualFlowRateDelta, time);
            SuperToken_universal_indexes.write(_from, newUIndexForFrom);
            SuperToken_universal_indexes.write(pool, _newUIndexForPool);
            ISuperfluidPool.operatorSetIndex(contract_address=pool, index=newPdIndex);
            _setFlowInfo(flowHash, _from, pool, newActualFlowRate, actualFlowRateDelta);
            _setPoolAdjustmentFlowRate(pool, FALSE, newAdjustmentFlowRate, time);
            return (newActualFlowRate=newActualFlowRate, newDistributionFlowRate=newDistributionFlowRate);
        } else {
            let newAdjustmentFlowRate = -newActualFlowRate;
            let newActualFlowRate = 0;
            let actualFlowRateDelta = newActualFlowRate - oldFlowRate;
            let (newUIndexForFrom, _newUIndexForPool) = SemanticMoney.shiftFlow2(uIndexForFrom, newUIndexForPool, actualFlowRateDelta, time);
            SuperToken_universal_indexes.write(_from, newUIndexForFrom);
            SuperToken_universal_indexes.write(pool, _newUIndexForPool);
            ISuperfluidPool.operatorSetIndex(contract_address=pool, index=newPdIndex);
            _setFlowInfo(flowHash, _from, pool, newActualFlowRate, actualFlowRateDelta);
            _setPoolAdjustmentFlowRate(pool, FALSE, newAdjustmentFlowRate, time);
            return (newActualFlowRate=newActualFlowRate, newDistributionFlowRate=newDistributionFlowRate);
        }
    }

    func connectPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(to: felt) -> (
        success: felt
    ) {
        alloc_locals;
        let (caller) = get_caller_address();
        let (poolIndex) = SuperToken_pool_indexes.read(to);
        let (connected) = SuperToken_connection_map.read(caller, poolIndex);
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
        let (poolIndex) = SuperToken_pool_indexes.read(to);
        let (connected) = SuperToken_connection_map.read(caller, poolIndex);
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
            let (connected) = ISuperfluidPool.operatorConnectMember(
                contract_address=pool, memberAddress=caller, dbConnect=TRUE
            );
            assert connected = TRUE;
            let (connectedPoolLength) = SuperToken_connected_pool_length.read(caller);
            SuperToken_connected_pool_length.write(caller, connectedPoolLength + 1);
        } else {
            SuperToken_connection_map.write(caller, poolIndex, FALSE);
            let (disconnected) = ISuperfluidPool.operatorConnectMember(
                contract_address=pool, memberAddress=caller, dbConnect=FALSE
            );
            assert disconnected = TRUE;
            let (connectedPoolLength) = SuperToken_connected_pool_length.read(caller);
            SuperToken_connected_pool_length.write(caller, connectedPoolLength - 1);
        }
        return (success=TRUE);
    }

    func setLiquadationPeriod{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(value: felt) {
        with_attr error_message("SuperToken: liquidation value is negative") {
            assert_nn(value);
        }
        SuperToken_liquidation_period.write(value);
        return ();
    }

    func _getFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(flowHash: felt) -> (flowRate: felt) {
        let (flowData) = SuperToken_flow_data.read(flowHash);
        return (flowRate=flowData.flowRate);
    }

    // // //////////////////////////////////////////////////////////////////////////////
    // // Generalized Payment Primitives ends here
    // // //////////////////////////////////////////////////////////////////////////////

    // // //////////////////////////////////////////////////////////////////////////////
    // ///////// Pool Operations starts here
    // // //////////////////////////////////////////////////////////////////////////////

    func isMemberConnected{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        pool: felt, memberAddress: felt
    ) -> (success: felt) {
        let (poolIndex) = SuperToken_pool_indexes.read(pool);
        let (isMemberConnected) = SuperToken_connection_map.read(memberAddress, poolIndex);
        return (success=isMemberConnected);
    }

    func getPoolAdjustmentFlowInfo{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt) -> (adjustmentRecipient: felt, flowHash: felt, flowRate: felt){
        return _getPoolAdjustmentFlowInfo(pool);
    }

    func _getPoolAdjustmentFlowInfo{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt) -> (adjustmentRecipient: felt, flowHash: felt, flowRate: felt){
        let (adjustmentRecipient) = ISuperfluidPool.admin(contract_address=pool);
        let (flowHash) = getPoolAdjustmentFlowHash(pool, adjustmentRecipient);
        let (flowData) = SuperToken_flow_data.read(flowHash);
        return (adjustmentRecipient=adjustmentRecipient, flowHash=flowHash, flowRate=flowData.flowRate);
    }

    func _getPoolAdjustmentFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt) -> (flowRate: felt){
        let (_,_,flowRate) = _getPoolAdjustmentFlowInfo(pool);
        return (flowRate=flowRate);
    }

    func _setPoolAdjustmentFlowRate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt, shiftFlow: felt, flowRate: felt, time: felt){
        let (adjustmentRecipient) = ISuperfluidPool.admin(contract_address=pool);
        let (adjustmentFlowHash) = getPoolAdjustmentFlowHash(pool, adjustmentRecipient);
        if (shiftFlow == TRUE){
            let (oldFlowRate) = _getFlowRate(adjustmentFlowHash);
            let _flowRate = flowRate + oldFlowRate;
            _doFlow(pool, adjustmentRecipient, adjustmentFlowHash, _flowRate, time);
        } else {
            _doFlow(pool, adjustmentRecipient, adjustmentFlowHash, flowRate, time);
        }   
        return ();
    }

    func appendIndexUpdateByPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(particle: BasicParticle, time: felt) -> (success: felt) {
        let (caller) = get_caller_address();
        _appendIndexUpdateByPool(caller, particle, time);
        return (success=TRUE);
    }

    func _appendIndexUpdateByPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool: felt, particle: BasicParticle, time: felt){
        let (isAPool) = isPool(pool);
        with_attr error_message("SuperToken: Only a pool can adjust flow!") {
            assert isAPool = TRUE;
        }
        let (uIndexForPool) = SuperToken_universal_indexes.read(pool);
        let (newUIndexForPool) = SemanticMoney.mappend(uIndexForPool, particle);
        SuperToken_universal_indexes.write(pool, newUIndexForPool);
        let (flowRate) = SemanticMoney.flow_rate(particle);
        _setPoolAdjustmentFlowRate(pool, TRUE, flowRate, time);
        return ();
    }

    func poolSettleClaim{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(claimRecipient: felt, amount: felt) -> (success: felt){
        let (caller) = get_caller_address();
        _poolSettleClaim(caller, claimRecipient, amount);
        return (success=TRUE);
    }

    func _poolSettleClaim{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(pool:felt, claimRecipient: felt, amount: felt){
        let (isAPool) = isPool(pool);
        with_attr error_message("SuperToken: Only a pool can settle claim!") {
            assert isAPool = TRUE;
        }
        _doShift(pool, claimRecipient, amount);
        return ();
    }
    
    func isPool{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(address: felt) -> (success: felt) {
        let (poolIndex) = SuperToken_pool_indexes.read(address);
        let (is_pool) = SuperToken_pools.read(poolIndex);
        if (is_pool == FALSE){
            return (success=FALSE);
        } else {
            return (success=TRUE);
        }
    }

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

    func getNumConnections{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        account: felt
    ) -> (value: felt) {
        return SuperToken_connected_pool_length.read(account);
    }

    ///////////////////////////////////////////////////////////////////////////////////
    //////////// Pool Operations ends here
    ///////////////////////////////////////////////////////////////////////////////////
    
    ////////////////////////////////////////////////////////////////////////////////////
    /////////// Buffer Solvency
    ////////////////////////////////////////////////////////////////////////////////////

    func _adjustBuffer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(_from: felt, flow_hash: felt, newflowRate: felt){
        alloc_locals;
        let (lp) = SuperToken_liquidation_period.read();
        let newBufferAmount = newflowRate * lp;
        let (flowData) = SuperToken_flow_data.read(flow_hash);
        let bufferDelta = newBufferAmount - flowData.buffer;
        let (contract_address) = get_contract_address();
        _doShift(_from, contract_address, bufferDelta);

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

    func _setFlowInfo{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(flowHash: felt, _from: felt, to: felt, newflowRate: felt, flowRateDelta: felt){
        let (flowData) = SuperToken_flow_data.read(flowHash);
        let newFlowData = FlowData(flowData._from, flowData.to, newflowRate, flowData.buffer);
        SuperToken_flow_data.write(flowHash, newFlowData);

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
