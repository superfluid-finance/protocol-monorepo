# UUPSProxiable

## Functions

### getCodeAddress

```solidity
function getCodeAddress(
) public returns (address codeAddress)
```

Get current implementation code address.

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

### proxiableUUID

```solidity
function proxiableUUID(
) public returns (bytes32)
```

Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT.

### _updateCodeAddress

```solidity
function _updateCodeAddress(
    address newAddress
) internal
```

Update code address function.
     It is internal, so the derived contract could setup its own permission logic.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newAddress` | address |  |

## Events

### CodeUpdated

```solidity
event CodeUpdated(
    bytes32 uuid,
    address codeAddress
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `uuid` | bytes32 |  |
| `codeAddress` | address |  |

