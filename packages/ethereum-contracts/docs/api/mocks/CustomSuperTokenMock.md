# CustomSuperTokenBaseMock

## Functions

### getFirstCustomTokenStorageSlot

```solidity
function getFirstCustomTokenStorageSlot(
) external returns (uint256 slot)
```

### callSelfBurn

```solidity
function callSelfBurn(
    address to,
    uint256 amount,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### callSelfTransferFrom

```solidity
function callSelfTransferFrom(
    address holder,
    address spender,
    address recipient,
    uint256 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `holder` | address |  |
| `spender` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

### callSelfApproveFor

```solidity
function callSelfApproveFor(
    address account,
    address spender,
    uint256 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `spender` | address |  |
| `amount` | uint256 |  |

# CustomSuperTokenMock

# CustomSuperTokenProxyMock

## Functions

### getFirstCustomTokenStorageSlot

```solidity
function getFirstCustomTokenStorageSlot(
) external returns (uint256 slot)
```

### selfMint

```solidity
function selfMint(
    address to,
    uint256 amount,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### delegatecallSelfMint

```solidity
function delegatecallSelfMint(
    address to,
    uint256 amount,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### callSelfBurn

```solidity
function callSelfBurn(
    address to,
    uint256 amount,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### callSelfTransferFrom

```solidity
function callSelfTransferFrom(
    address holder,
    address spender,
    address recipient,
    uint256 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `holder` | address |  |
| `spender` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

### callSelfApproveFor

```solidity
function callSelfApproveFor(
    address account,
    address spender,
    uint256 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `spender` | address |  |
| `amount` | uint256 |  |

