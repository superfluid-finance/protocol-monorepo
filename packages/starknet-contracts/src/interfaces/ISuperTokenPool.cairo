%lang starknet

from src.utils.SemanticMoney import PDPoolIndex, PDPoolMember

@contract_interface
namespace ISuperTokenPool {
    func getTotalUnits() -> (value: felt) {
    }

    func getDistributionFlowRate() -> (flow_rate: felt) {
    }

    func getPendingDistributionFlowRate() -> (flow_rate: felt) {
    }

    func getPendingUnits() -> (value: felt) {
    }

    func getPendingDistribution() -> (value: felt) {
    }

    func getIndex() -> (index: PDPoolIndex) {
    }

    func getUnits(memberAddress: felt) -> (value: felt) {
    }

    func getMemberFlowRate(memberAddress: felt) -> (flow_rate: felt) {
    }

    func getClaimable(time: felt, memberAddress: felt) -> (value: felt) {
    }

    func claimAll() -> (success: felt) {
    }

    func updateMember(memberAddress: felt, unit: felt) -> (success: felt) {
    }

    func operatorSetIndex(index: PDPoolIndex) -> (success: felt) {
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    func operatorConnectMember(memberAddress: felt, dbConnect: felt) -> (success: felt) {
    }

    func getMember(memberAddress: felt) -> (member_data: PDPoolMember) {
    }
}
