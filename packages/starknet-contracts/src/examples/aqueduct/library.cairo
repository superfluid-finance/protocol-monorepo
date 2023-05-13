%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem
from starkware.cairo.common.bool import TRUE, FALSE

from src.interfaces.ISuperfluidToken import ISuperfluidToken
from src.interfaces.ISuperfluidPool import ISuperfluidPool

namespace AqueductLibrary {
    struct SideState {
        totalInFlowRate: felt,
    }

    func clone{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: SideState) -> (
        b: SideState
    ) {
        let sideState = SideState(a.totalInFlowRate);
        return (b=sideState);
    }

    func update_side{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: SideState, curUnitsB: felt, r0: felt, r1: felt
    ) -> (b: SideState, newDistFlowRateA: felt, newUnitsB: felt) {
        let newTotalFlowRate = a.totalInFlowRate + r1 - r0;
        let b = SideState(newTotalFlowRate);
        let newDistFlowRateA = newTotalFlowRate;
        // TODO: Review the usefulness of this
        let newUnitsB = curUnitsB + r1 - r0;
        return (b=b, newDistFlowRateA=newDistFlowRateA, newUnitsB=newUnitsB);
    }

    func cal_new_flowrate_A{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        totalA: felt, totalB: felt, oldFlowRateB: felt, newFlowRateB: felt
    ) -> (newFlowRateA: felt) {
        let (newFlowRateA, _) = unsigned_div_rem(totalA * newFlowRateB, ((totalB + newFlowRateB) - oldFlowRateB));
        return (newFlowRateA=newFlowRateA);
    }

    func cal_new_flowrate_B{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        totalA: felt, totalB: felt, oldFlowRateA: felt, newFlowRateA: felt
    ) -> (newFlowRateB: felt) {
        let (newFlowRateB, _) = unsigned_div_rem(totalB * newFlowRateA, (totalA + newFlowRateA) - oldFlowRateA);
        return (newFlowRateB=newFlowRateB);
    }
}

struct Side {
    superToken: felt,
    superTokenPool: felt,
    state: AqueductLibrary.SideState,
}

@storage_var
func _left() -> (res: Side) {
}

@storage_var
func _right() -> (res: Side) {
}

namespace Aqueduct {

    const SWAP_DISTRIBUTE_FLOW_ID = 0;
    const ADJUSTMENT_FLOW_ID = 0;

    func initializer{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        tokenL: felt, tokenR: felt
    ) {
        let (pool) = ISuperfluidToken.createPool(contract_address=tokenL);
        let (left) = _left.read();
        let newLeft = Side(tokenL, pool, left.state);
        _left.write(newLeft);


        let (pool) = ISuperfluidToken.createPool(contract_address=tokenR);
        let (right) = _right.read();
        let newRight= Side(tokenR, pool, right.state);
        _right.write(newRight);
        return ();
    }

    func token1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        token: felt
    ) {
        let (left) = _left.read();
        return (token=left.superToken);
    }

    func token2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (
        token: felt
    ) {
        let (right) = _right.read();
        return (token=right.superToken);
    }

    func pool1{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (pool: felt) {
        let (left) = _left.read();
        return (pool=left.superTokenPool);
    }

    func pool2{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}() -> (pool: felt) {
        let (right) = _right.read();
        return (pool=right.superTokenPool);
    }

    func onFlowUpdate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(token: felt, _from: felt, ir0: felt, ir1: felt) {
        let (left) = _left.read();
        let (right) = _right.read();
        if(token == left.superToken){
            _onFlowUpdate(left, right, _from, ir0, ir1, TRUE);
        } else {
            if(token == right.superToken){
                _onFlowUpdate(right, left, _from, ir0, ir1, FALSE);
            } else {
                return ();
            }
        }
        return ();
    }

    func _onFlowUpdate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: Side, b: Side, _from: felt, ir0: felt, ir1: felt, update_left: felt) {
        alloc_locals;
        // Update units based on new flowrate
        let (curUnitsB) = ISuperfluidPool.getUnits(contract_address=b.superTokenPool, memberAddress=_from);
        let (newSideStateB, _, unit) =  AqueductLibrary.update_side(b.state, curUnitsB, ir0, ir1);
        let newSide = Side(b.superToken, b.superTokenPool, newSideStateB);
        if (update_left == TRUE){
            _left.write(newSide);
        } else {
            _right.write(newSide);
        }
        ISuperfluidPool.updateMember(contract_address=b.superTokenPool, memberAddress=_from, unit=unit);
        return ();
    }
}
