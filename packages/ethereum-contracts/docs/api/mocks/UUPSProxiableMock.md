# UUPSProxiableMock

## Functions

### constructor

```solidity
function constructor(
    bytes32 uuid,
    uint256 w
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `uuid` | bytes32 |  |
| `w` | uint256 |  |

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

