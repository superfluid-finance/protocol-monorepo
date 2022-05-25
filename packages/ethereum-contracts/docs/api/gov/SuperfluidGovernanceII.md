# SuperfluidGovernanceIIProxy

# SuperfluidGovernanceII

## Functions

### _requireAuthorised

```solidity
function _requireAuthorised(
) private
```

### proxiableUUID

```solidity
function proxiableUUID(
) public returns (bytes32)
```

Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT.

### updateCode

```solidity
function updateCode(
    address newAddress
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newAddress` | address |  |

### _requireAuthorised

```solidity
function _requireAuthorised(
    contract ISuperfluid 
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperfluid |  |

