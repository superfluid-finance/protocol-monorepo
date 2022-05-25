# BaseRelayRecipient

A base contract to be inherited by any contract that want to receive relayed transactions
     A subclass must use "_msgSender()" instead of "msg.sender"
     MODIFIED FROM: https://github.com/opengsn/forwarder/blob/master/contracts/BaseRelayRecipient.sol

## Functions

### isTrustedForwarder

```solidity
function isTrustedForwarder(
    address forwarder
) public returns (bool)
```

Check if the forwarder is trusted

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `forwarder` | address |  |

### _getTransactionSigner

```solidity
function _getTransactionSigner(
) internal returns (address payable ret)
```

Return the transaction signer of this call

if the call came through our trusted forwarder, return the original sender.
otherwise, return `msg.sender`.
should be used in the contract anywhere instead of msg.sender

