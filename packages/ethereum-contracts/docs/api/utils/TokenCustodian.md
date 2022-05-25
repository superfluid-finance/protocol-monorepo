# TokenCustodian

Contract which takes custody of funds which couldn't be sent to the designated recipient

## Functions

### constructor

```solidity
function constructor(
) public
```

### flush

```solidity
function flush(
    contract IERC777 token,
    address recipient
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract IERC777 |  |
| `recipient` | address |  |

### tokensReceived

```solidity
function tokensReceived(
    address ,
    address ,
    address ,
    uint256 amount,
    bytes userData,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | address |  |
| `` | address |  |
| `` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `` | bytes |  |

## Events

### CustodianDeposit

```solidity
event CustodianDeposit(
    contract IERC777 token,
    address recipient,
    uint256 amount
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract IERC777 |  |
| `recipient` | address |  |
| `amount` | uint256 |  |
### CustodianWithdrawal

```solidity
event CustodianWithdrawal(
    contract IERC777 token,
    address recipient,
    uint256 amount
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract IERC777 |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

