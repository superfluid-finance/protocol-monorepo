%lang starknet

from src.utils.SemanticMoney import PDPoolMember, PDPoolIndex

@contract_interface
namespace ISuperTokenPool {
    func getPendingDistribution() -> (value: felt) {
    }

    func getIndex() -> (index: PDPoolIndex) {
    }

    func getClaimable(time: felt, memberAddress: felt) -> (value: felt) {
    }

    func claimAll(time: felt, memberAddress: felt) -> (success: felt) {
    }

    func updateMember(memberAddress: felt, unit: felt) -> (success: felt) {
    }

    func operatorSetIndex(index: PDPoolIndex) -> (success: felt) {
    }

    // WARNING for operators: it is undefined behavior if member is already connected or disconnected
    func operatorConnectMember(time: felt, memberAddress: felt, dbConnect: felt) -> (
        success: felt
    ) {
    }
}
