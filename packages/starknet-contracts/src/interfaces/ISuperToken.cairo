%lang starknet

struct RTBParticle {
    settledAt: felt,
    settedValue: felt,
    flowRate: felt,
}

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

    func createFlow(universalIndex: RTBParticle, receiver: felt) {
    }

    func updateFlow(universalIndex: RTBParticle, receiver: felt) {
    }

    func deleteFlow(universalIndex: RTBParticle, receiver: felt) {
    }

    func distribute(pdIndex: RTBParticle, amount: felt) {
    }

    func distributeFlow(pdIndex: RTBParticle, pdPoolMember_len: felt, pdPoolMember: RTBParticle*, amount: felt) {
    }

    func approve(spender: felt, amount: felt) -> (success: felt) {
    }
}