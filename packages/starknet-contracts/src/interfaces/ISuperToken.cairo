%lang starknet

from src.utils.SemanticMoney import BasicParticle

@contract_interface
namespace ISuperToken {
    func name() -> (name: felt) {
    }

    func symbol() -> (symbol: felt) {
    }

    func decimals() -> (decimals: felt) {
    }

    func totalSupply() -> (totalSupply: felt) {
    }

    func balanceOf(account: felt) -> (balance: felt) {
    }

    func allowance(owner: felt, spender: felt) -> (remaining: felt) {
    }

    func transfer(receiver: felt, amount: felt) -> (success: felt) {
    }

    func transferFrom(sender: felt, receiver: felt, amount: felt) -> (success: felt) {
    }

    func mint(receiver: felt, amount: felt) {
    }

    func approve(spender: felt, amount: felt) -> (success: felt) {
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    // //////////////////////////////////////////////////////////////////////////////

    func realtimeBalanceOf(account: felt) -> (rtb: felt) {
    }

    func realtimeBalanceAt(account: felt, time: felt) -> (rtb: felt) {
    }

    func realtimeBalanceVectorAt(account: felt, time: felt) -> (available: felt, deposit: felt) {
    }

    func getNetFlowRate(account: felt) -> (flow_rate: felt) {
    }

    func getFlowRate(_from: felt, to: felt, flowId: felt) -> (flow_rate: felt) {
    }

    func shift(senderAddress: felt, receiverAddress: felt, amount: felt) -> (success: felt) {
    }

    func flow(senderAddress: felt, receiverAddress: felt, flowId: felt, flowRate: felt) -> (
        success: felt
    ) {
    }

    func distribute(senderAddress: felt, poolAddress: felt, reqAmount: felt) -> (
        success: felt, actualAmount: felt
    ) {
    }

    func distributeFlow(
        senderAddress: felt, poolAddress: felt, flowId: felt, reqFlowRate: felt
    ) -> (success: felt, actualFlowRate: felt) {
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    // //////////////////////////////////////////////////////////////////////////////

    func connectPool(to: felt) -> (success: felt) {
    }

    func disconnectPool(to: felt) -> (success: felt) {
    }

    func connectPoolEnum(to: felt, connect: felt) -> (success: felt) {
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Pool Owner Operations
    // //////////////////////////////////////////////////////////////////////////////

    func createPool() -> (pool: felt) {
    }

    func getNumConnections(account: felt) -> (value: felt) {
    }
}
