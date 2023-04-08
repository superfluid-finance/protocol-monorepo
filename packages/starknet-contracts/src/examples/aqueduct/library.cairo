%lang starknet

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.math import unsigned_div_rem

from src.interfaces.ISuperToken import ISuperToken
from src.interfaces.ISuperTokenPool import ISuperTokenPool

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

    func updateSide{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        a: SideState, curUnitsB: felt, r0: felt, r1: felt
    ) -> (b: SideState, newDistFlowRateA: felt, newUnitsB: felt) {
        let (b) = clone(a);
        b.totalInFlowRate = a.totalInFlowRate + r1 - r0;
        let newDistFlowRateA = b.totalInFlowRate;
        let newUnitsB = curUnitsB + r1 - r0;
        return (b=b, newDistFlowRateA=newDistFlowRateA, newUnitsB=newUnitsB);
    }

    func calNewFlowRateA{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        totalA: felt, totalB: felt, newFlowRateB: felt
    ) -> (newFlowRateA: felt) {
        let (newFlowRateA, _) = unsigned_div_rem(totalA * newFlowRateB, totalB);
        return (newFlowRateA=newFlowRateA);
    }

    func calNewFlowRateB{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(
        totalA: felt, totalB: felt, newFlowRateA: felt
    ) -> (newFlowRateB: felt) {
        let (newFlowRateB, _) = unsigned_div_rem(totalB * newFlowRateA, totalA);
        return (newFlowRateB=newFlowRateB);
    }
}

struct Side {
    superToken: felt,
    superTokenPool: felt,
    state: AqueductLibrary.SideState,
    remFlowReceiver: felt,
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
        let (left) = _left.read();
        left.superToken = tokenL;
        let (pool) = ISuperToken.createPool(contract_address=tokenL);
        left.superTokenPool = pool;
        _left.write(left);

        let (right) = _right.read();
        right.superToken = tokenR;
        let (pool) = ISuperToken.createPool(contract_address=tokenR);
        right.superTokenPool = pool;
        _right.write(left);
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

    func addLiquidity{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(){
        
        return ();
    }

    // func onFlowUpdate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(token: felt, _from: felt, ir0: felt, ir1: felt) {
    //         let (left) = _left.read();
    //         let (right) = _right.read();
    //         if(token == left.token){
    //              _onFlowUpdate(left, right, _from, ir0, ir1);
    //         }
    //         if (token == right.token) {
    //              _onFlowUpdate(right, left, _from, ir0, ir1);
    //         } else {
    //             return ();
    //         }
    //         return ();
    //     }

    // func _onFlowUpdate{syscall_ptr: felt*, pedersen_ptr: HashBuiltin*, range_check_ptr}(a: Side, b: Side, _from: felt, ir0: felt, ir1: felt) {
    //         let (curUnits) = ISuperTokenPool.getUnits(contract_address=b.superTokenPool, memberAddress=_from);
    //         let (sideStateA, newDistFlowRateA, newUnitsB) = AqueductLibrary.updateSide(a, curUnits, ir0, ir1);
    //         a.state = sideStateA;
    //         let contract_address
    //         let flowRateForA = ISuperToken.getFlowRate(contract_address=)
    //         FlowRate ar0 = a.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
    //         FlowRate dr0 = a.pool.getDistributionFlowRate();
    //         _adjustFlowRemainder(a, from, ir1 - ir0, drr, ar0, dr0);

    // FlowRate ar0 = b.token.getFlowRate(address(this), from, ADJUSTMENT_FLOW_ID);
    //         FlowRate dr0 = b.pool.getDistributionFlowRate();
    //         b.pool.updateMember(from, u1);
    //         _adjustFlowRemainder(b, from, FlowRate.wrap(0), dr0 + ar0, ar0, dr0);
    //         return ();
    //     }
}
