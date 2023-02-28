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

    func approve(spender: felt, amount: felt) -> (success: felt) {
    }

    func iTransfer(senderAddress: felt, receiverAddress: felt, amount: felt){
    }

    func flow(senderAddress: felt, receiverAddress: felt, flowId: felt, flowRate: felt) -> (success: felt) {
    }

    func distribute(senderAddress: felt, poolAddress: felt, reqAmount: felt) {
    }

    func distributeFlow(senderAddress: felt, receiverAddress: felt, flowId: felt, reqFlowRate: felt) -> (success: felt, actualFlowRate: felt){
    }

    func connectPool(to: felt) -> (success: felt) {
    }

    func disconnectPool(to: felt) -> (success: felt) {
    }

    func connectPoolEnum(to: felt, connect: felt) -> (success: felt) {
    }

    func createPool() -> (pool: felt) {
    }

    func absorb(account: felt, particle: BasicParticle) {
    }

}
