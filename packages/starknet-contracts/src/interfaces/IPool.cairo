%lang starknet

from src.utils.SemanticMoney import PDPoolMember, PDPoolIndex

@contract_interface
namespace IPool {

    func getIndex() -> (index: PDPoolIndex) {
    }

    func setIndex(index: PDPoolIndex ) {
    }

    func getMember(memberAddress: felt) -> (member: PDPoolMember) {
    }

    func updatePoolMember(memberAddress: felt, unit: felt){
    }
}