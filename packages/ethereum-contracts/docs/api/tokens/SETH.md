# SETHProxy

Super ETH (SETH) custom super token implementation

## Functions

### receive

```solidity
function receive(
) external
```

Fallback function that delegates calls to the address returned by `_implementation()`. Will run if call data
is empty.

### upgradeByETH

```solidity
function upgradeByETH(
) external
```

### upgradeByETHTo

```solidity
function upgradeByETHTo(
    address to
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |

### downgradeToETH

```solidity
function downgradeToETH(
    uint256 wad
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `wad` | uint256 |  |

## Events

### TokenUpgraded

```solidity
event TokenUpgraded(
    address account,
    uint256 amount
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |
### TokenDowngraded

```solidity
event TokenDowngraded(
    address account,
    uint256 amount
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |

