# Solidity API

## BaseRelayRecipient

_A base contract to be inherited by any contract that want to receive relayed transactions
     A subclass must use &quot;_msgSender()&quot; instead of &quot;msg.sender&quot;
     MODIFIED FROM: https://github.com/opengsn/forwarder/blob/master/contracts/BaseRelayRecipient.sol_

### isTrustedForwarder

```solidity
function isTrustedForwarder(address forwarder) public view virtual returns (bool)
```

_Check if the forwarder is trusted_

### _getTransactionSigner

```solidity
function _getTransactionSigner() internal view virtual returns (address payable ret)
```

_Return the transaction signer of this call

if the call came through our trusted forwarder, return the original sender.
otherwise, return &#x60;msg.sender&#x60;.
should be used in the contract anywhere instead of msg.sender_

