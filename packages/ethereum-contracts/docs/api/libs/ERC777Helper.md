# ERC777Helper

## Functions

### register

```solidity
function register(
    address token
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | address |  |

### isOperatorFor

```solidity
function isOperatorFor(
    struct ERC777Helper.Operators self,
    address operator,
    address tokenHolder
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `self` | struct ERC777Helper.Operators |  |
| `operator` | address |  |
| `tokenHolder` | address |  |

### authorizeOperator

```solidity
function authorizeOperator(
    struct ERC777Helper.Operators self,
    address holder,
    address operator
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `self` | struct ERC777Helper.Operators |  |
| `holder` | address |  |
| `operator` | address |  |

### revokeOperator

```solidity
function revokeOperator(
    struct ERC777Helper.Operators self,
    address holder,
    address operator
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `self` | struct ERC777Helper.Operators |  |
| `holder` | address |  |
| `operator` | address |  |

### defaultOperators

```solidity
function defaultOperators(
    struct ERC777Helper.Operators self
) internal returns (address[])
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `self` | struct ERC777Helper.Operators |  |

### setupDefaultOperators

```solidity
function setupDefaultOperators(
    struct ERC777Helper.Operators self,
    address[] operators
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `self` | struct ERC777Helper.Operators |  |
| `operators` | address[] |  |

