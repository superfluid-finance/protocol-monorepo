%lang starknet

from src.utils.SemanticMoney import PDPoolIndex, PDPoolMember, BasicParticle

///////////////////////////////////////////////////////////
////// The interface for any super token pool regardless of the distribution schemes.
///////////////////////////////////////////////////////////

@contract_interface
namespace ISuperfluidPool {
    func admin() -> (address: felt) {
    }

    func getIndex() -> (index: PDPoolIndex) {
    }

    func getTotalUnits() -> (value: felt) {
    }

    func getDisconnectedUnits() -> (unit: felt) {
    }

    func getUnits(memberAddress: felt) -> (value: felt) {
    }

    func getDistributionFlowRate() -> (flow_rate: felt) {
    }

    func getConnectedFlowRate() -> (flow_rate: felt) {
    }

    func getDisconnectedFlowRate() -> (flow_rate: felt) {
    }

    func getDisconnectedBalance(time: felt) -> (value: felt) {
    }

    func getMemberFlowRate(memberAddress: felt) -> (flow_rate: felt) {
    }

    func getClaimable(time: felt, memberAddress: felt) -> (value: felt) {
    }

    func updateMember(memberAddress: felt, unit: felt) -> (success: felt) {
    }

    func claimAll() -> (success: felt) {
    }

    func operatorSetIndex(index: PDPoolIndex) -> (success: felt) {
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    func operatorConnectMember(memberAddress: felt, dbConnect: felt) -> (success: felt) {
    }

    func getMember(memberAddress: felt) -> (member_data: PDPoolMember) {
    }
}

 ///////////////////////////////////////////////////////////
 ////// The interface for the operator of a super token pool
 ///////////////////////////////////////////////////////////

@contract_interface
namespace ISuperfluidPoolOperator {
    /// Check if an address is connected to the pool
    func isMemberConnected(pool: felt, memberAddress: felt) -> (success: felt) {
    }

    /// Get pool adjustment flow information: (recipient, flowHahs, flowRate)
    func getPoolAdjustmentFlowInfo(pool: felt) -> (address: felt, flow_hash: felt, flow_rate: felt) {
    }

    /// Update the adjustment flow rate
    func appendIndexUpdateByPool(particle: BasicParticle, time: felt) -> (success: felt) {
    }

    /// Settle the claim
    func poolSettleClaim(claimRecipient: felt, amount: felt) -> (success: felt) {
    }
}
