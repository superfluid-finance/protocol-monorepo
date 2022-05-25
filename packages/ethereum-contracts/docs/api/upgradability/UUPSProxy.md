# UUPSProxy

## Functions

### initializeProxy

```solidity
function initializeProxy(
    address initialAddress
) external
```

Proxy initialization function.
     This should only be called once and it is permission-less.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `initialAddress` | address | Initial logic contract code address to be used. |

### _implementation

```solidity
function _implementation(
) internal returns (address)
```

Proxy._implementation implementation

