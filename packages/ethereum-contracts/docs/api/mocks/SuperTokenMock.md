# SuperTokenStorageLayoutTester

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

### validateStorageLayout

```solidity
function validateStorageLayout(
) external
```

### getLastSuperTokenStorageSlot

```solidity
function getLastSuperTokenStorageSlot(
) external returns (uint256 slot)
```

# SuperTokenMock

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    uint256 w
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `w` | uint256 |  |

### approveInternal

```solidity
function approveInternal(
    address owner,
    address spender,
    uint256 value
) external
```

ERC-20 mockings

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `owner` | address |  |
| `spender` | address |  |
| `value` | uint256 |  |

### transferInternal

```solidity
function transferInternal(
    address from,
    address to,
    uint256 value
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `from` | address |  |
| `to` | address |  |
| `value` | uint256 |  |

### setupDefaultOperators

```solidity
function setupDefaultOperators(
    address[] operators
) external
```

ERC-777 mockings

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operators` | address[] |  |

### mintInternal

```solidity
function mintInternal(
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

