# Solidity API

## UUPSProxy

### initializeProxy

```solidity
function initializeProxy(address initialAddress) external
```

_Proxy initialization function.
     This should only be called once and it is permission-less._

| Name | Type | Description |
| ---- | ---- | ----------- |
| initialAddress | address | Initial logic contract code address to be used. |

### _implementation

```solidity
function _implementation() internal view virtual returns (address)
```

_Proxy._implementation implementation_

