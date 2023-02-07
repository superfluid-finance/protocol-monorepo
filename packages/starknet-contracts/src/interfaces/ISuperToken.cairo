%lang starknet

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

    func createFlow(sender: felt, receiver: felt) {
    }

    func updateFlow(sender: felt, receiver: felt) {
    }

    func deleteFlow(sender: felt, receiver: felt) {
    }

    func approve(spender: felt, amount: felt) -> (success: felt) {
    }
}