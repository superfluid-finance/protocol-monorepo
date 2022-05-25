# FixedSizeData

## Functions

### storeData

```solidity
function storeData(
    bytes32 slot,
    bytes32[] data
) internal
```

Store data to the slot at `slot`

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `slot` | bytes32 |  |
| `data` | bytes32[] |  |

### hasData

```solidity
function hasData(
    bytes32 slot,
    uint256 dataLength
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `slot` | bytes32 |  |
| `dataLength` | uint256 |  |

### loadData

```solidity
function loadData(
    bytes32 slot,
    uint256 dataLength
) internal returns (bytes32[] data)
```

Load data of size `dataLength` from the slot at `slot`

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `slot` | bytes32 |  |
| `dataLength` | uint256 |  |

### eraseData

```solidity
function eraseData(
    bytes32 slot,
    uint256 dataLength
) internal
```

Erase data of size `dataLength` from the slot at `slot`

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `slot` | bytes32 |  |
| `dataLength` | uint256 |  |

