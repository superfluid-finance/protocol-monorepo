# TestToken

Test ERC20 token that allows any one mint new tokens.

## Functions

### constructor

```solidity
function constructor(
    string name,
    string symbol,
    uint8 initDecimals
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `name` | string |  |
| `symbol` | string |  |
| `initDecimals` | uint8 |  |

### mint

```solidity
function mint(
    address account,
    uint256 amount
) public returns (bool)
```

See {ERC20-_mint}.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |

### decimals

```solidity
function decimals(
) public returns (uint8)
```

Returns the number of decimals used to get its user representation.
For example, if `decimals` equals `2`, a balance of `505` tokens should
be displayed to a user as `5.05` (`505 / 10 ** 2`).

Tokens usually opt for a value of 18, imitating the relationship between
Ether and Wei. This is the value {ERC20} uses, unless this function is
overridden;

NOTE: This information is only used for _display_ purposes: it in
no way affects any of the arithmetic of the contract, including
{IERC20-balanceOf} and {IERC20-transfer}.

