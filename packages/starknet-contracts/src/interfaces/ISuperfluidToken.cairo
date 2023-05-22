%lang starknet

from src.utils.SemanticMoney import BasicParticle

@contract_interface
namespace ISuperfluidToken {
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

    func transfer(to: felt, amount: felt) -> (success: felt) {
    }

    func transferFrom(_from: felt, to: felt, amount: felt) -> (success: felt) {
    }

    func mint(receiver: felt, amount: felt) {
    }

    func approve(spender: felt, amount: felt) -> (success: felt) {
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Generalized Payment Primitives
    // //////////////////////////////////////////////////////////////////////////////

    func realtimeBalanceNow(account: felt) -> (rtb: felt) {
    }

    func realtimeBalanceAt(account: felt, time: felt) -> (rtb: felt) {
    }

    func realtimeBalanceVectorNow(account: felt) -> (own: felt, fromPool: felt, deposit: felt) {
    }

    func realtimeBalanceVectorAt(account: felt, time: felt) -> (own: felt, fromPool: felt, deposit: felt) {
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
    ) -> (success: felt, actualFlowRate: felt, newDistributionFlowRate: felt) {
    }

    // //////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    // //////////////////////////////////////////////////////////////////////////////

    func isPool(address: felt) -> (success: felt) {
    }

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

    /// Check if an address is connected to the pool
    func isMemberConnected(pool: felt, memberAddress: felt) -> (success: felt) {
    }

    /// Get pool adjustment flow information: (recipient, flowHash, flowRate)
    func getPoolAdjustmentFlowInfo(pool: felt) -> (adjustmentRecipient: felt, flowHash: felt, flowRate: felt) {
    }

    /// Update the adjustment flow rate
    func appendIndexUpdateByPool(particle: BasicParticle, time: felt) -> (success: felt) {
    }

    /// Settle the claim
    func poolSettleClaim(claimRecipient: felt, amount: felt) -> (success: felt) {
    }
}
