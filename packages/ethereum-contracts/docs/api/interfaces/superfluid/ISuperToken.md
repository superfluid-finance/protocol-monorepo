# ISuperToken

## Functions

### initialize

```solidity
function initialize(
    contract IERC20 underlyingToken,
    uint8 underlyingDecimals,
    string n,
    string s
) external
```

Initialize the contract

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `underlyingToken` | contract IERC20 |  |
| `underlyingDecimals` | uint8 |  |
| `n` | string |  |
| `s` | string |  |

### name

```solidity
function name(
) external returns (string)
```

Returns the name of the token.

### symbol

```solidity
function symbol(
) external returns (string)
```

Returns the symbol of the token, usually a shorter version of the
name.

### decimals

```solidity
function decimals(
) external returns (uint8)
```

Returns the number of decimals used to get its user representation.
For example, if `decimals` equals `2`, a balance of `505` tokens should
be displayed to a user as `5,05` (`505 / 10 ** 2`).

Tokens usually opt for a value of 18, imitating the relationship between
Ether and Wei. This is the value {ERC20} uses, unless {_setupDecimals} is
called.

NOTE: SuperToken always uses 18 decimals.

Note: This information is only used for _display_ purposes: it in
no way affects any of the arithmetic of the contract, including
{IERC20-balanceOf} and {IERC20-transfer}.

### totalSupply

```solidity
function totalSupply(
) external returns (uint256)
```

See {IERC20-totalSupply}.

### balanceOf

```solidity
function balanceOf(
    address account
) external returns (uint256 balance)
```

Returns the amount of tokens owned by an account (`owner`).

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### transfer

```solidity
function transfer(
    address recipient,
    uint256 amount
) external returns (bool)
```

Moves `amount` tokens from the caller's account to `recipient`.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `recipient` | address |  |
| `amount` | uint256 |  |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bool | Returns Success a boolean value indicating whether the operation succeeded.

Emits a {Transfer} event. |

### allowance

```solidity
function allowance(
    address owner,
    address spender
) external returns (uint256)
```

Returns the remaining number of tokens that `spender` will be
        allowed to spend on behalf of `owner` through {transferFrom}. This is
        zero by default.

This value changes when {approve} or {transferFrom} are called.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `owner` | address |  |
| `spender` | address |  |

### approve

```solidity
function approve(
    address spender,
    uint256 amount
) external returns (bool)
```

Sets `amount` as the allowance of `spender` over the caller's tokens.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `spender` | address |  |
| `amount` | uint256 |  |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bool | Returns Success a boolean value indicating whether the operation succeeded.

IMPORTANT: Beware that changing an allowance with this method brings the risk
that someone may use both the old and the new allowance by unfortunate
transaction ordering. One possible solution to mitigate this race
condition is to first reduce the spender's allowance to 0 and set the
desired value afterwards:
https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729

Emits an {Approval} event. |

### transferFrom

```solidity
function transferFrom(
    address sender,
    address recipient,
    uint256 amount
) external returns (bool)
```

Moves `amount` tokens from `sender` to `recipient` using the
        allowance mechanism. `amount` is then deducted from the caller's
        allowance.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bool | Returns Success a boolean value indicating whether the operation succeeded.

Emits a {Transfer} event. |

### increaseAllowance

```solidity
function increaseAllowance(
    address spender,
    uint256 addedValue
) external returns (bool)
```

Atomically increases the allowance granted to `spender` by the caller.

This is an alternative to {approve} that can be used as a mitigation for
problems described in {IERC20-approve}.

Emits an {Approval} event indicating the updated allowance.

Requirements:

- `spender` cannot be the zero address.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `spender` | address |  |
| `addedValue` | uint256 |  |

### decreaseAllowance

```solidity
function decreaseAllowance(
    address spender,
    uint256 subtractedValue
) external returns (bool)
```

Atomically decreases the allowance granted to `spender` by the caller.

This is an alternative to {approve} that can be used as a mitigation for
problems described in {IERC20-approve}.

Emits an {Approval} event indicating the updated allowance.

Requirements:

- `spender` cannot be the zero address.
- `spender` must have allowance for the caller of at least
`subtractedValue`.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `spender` | address |  |
| `subtractedValue` | uint256 |  |

### granularity

```solidity
function granularity(
) external returns (uint256)
```

Returns the smallest part of the token that is not divisible. This
        means all token operations (creation, movement and destruction) must have
        amounts that are a multiple of this number.

For super token contracts, this value is 1 always

### send

```solidity
function send(
    address recipient,
    uint256 amount,
    bytes data
) external
```

Moves `amount` tokens from the caller's account to `recipient`.

If send or receive hooks are registered for the caller and `recipient`,
     the corresponding functions will be called with `data` and empty
     `operatorData`. See {IERC777Sender} and {IERC777Recipient}.

Emits a {Sent} event.

Requirements

- the caller must have at least `amount` tokens.
- `recipient` cannot be the zero address.
- if `recipient` is a contract, it must implement the {IERC777Recipient}
interface.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `recipient` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |

### burn

```solidity
function burn(
    uint256 amount,
    bytes data
) external
```

Destroys `amount` tokens from the caller's account, reducing the
total supply.

If a send hook is registered for the caller, the corresponding function
will be called with `data` and empty `operatorData`. See {IERC777Sender}.

Emits a {Burned} event.

Requirements

- the caller must have at least `amount` tokens.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 |  |
| `data` | bytes |  |

### isOperatorFor

```solidity
function isOperatorFor(
    address operator,
    address tokenHolder
) external returns (bool)
```

Returns true if an account is an operator of `tokenHolder`.
Operators can send and burn tokens on behalf of their owners. All
accounts are their own operator.

See {operatorSend} and {operatorBurn}.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `tokenHolder` | address |  |

### authorizeOperator

```solidity
function authorizeOperator(
    address operator
) external
```

Make an account an operator of the caller.

See {isOperatorFor}.

Emits an {AuthorizedOperator} event.

Requirements

- `operator` cannot be calling address.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |

### revokeOperator

```solidity
function revokeOperator(
    address operator
) external
```

Revoke an account's operator status for the caller.

See {isOperatorFor} and {defaultOperators}.

Emits a {RevokedOperator} event.

Requirements

- `operator` cannot be calling address.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |

### defaultOperators

```solidity
function defaultOperators(
) external returns (address[])
```

Returns the list of default operators. These accounts are operators
for all token holders, even if {authorizeOperator} was never called on
them.

This list is immutable, but individual holders may revoke these via
{revokeOperator}, in which case {isOperatorFor} will return false.

### operatorSend

```solidity
function operatorSend(
    address sender,
    address recipient,
    uint256 amount,
    bytes data,
    bytes operatorData
) external
```

Moves `amount` tokens from `sender` to `recipient`. The caller must
be an operator of `sender`.

If send or receive hooks are registered for `sender` and `recipient`,
the corresponding functions will be called with `data` and
`operatorData`. See {IERC777Sender} and {IERC777Recipient}.

Emits a {Sent} event.

Requirements

- `sender` cannot be the zero address.
- `sender` must have at least `amount` tokens.
- the caller must be an operator for `sender`.
- `recipient` cannot be the zero address.
- if `recipient` is a contract, it must implement the {IERC777Recipient}
interface.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |
| `operatorData` | bytes |  |

### operatorBurn

```solidity
function operatorBurn(
    address account,
    uint256 amount,
    bytes data,
    bytes operatorData
) external
```

Destroys `amount` tokens from `account`, reducing the total supply.
The caller must be an operator of `account`.

If a send hook is registered for `account`, the corresponding function
will be called with `data` and `operatorData`. See {IERC777Sender}.

Emits a {Burned} event.

Requirements

- `account` cannot be the zero address.
- `account` must have at least `amount` tokens.
- the caller must be an operator for `account`.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |
| `operatorData` | bytes |  |

### selfMint

```solidity
function selfMint(
    address account,
    uint256 amount,
    bytes userData
) external
```

Mint new tokens for the account

Modifiers:
 - onlySelf

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### selfBurn

```solidity
function selfBurn(
    address account,
    uint256 amount,
    bytes userData
) external
```

Burn existing tokens for the account

Modifiers:
 - onlySelf

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### selfTransferFrom

```solidity
function selfTransferFrom(
    address sender,
    address spender,
    address recipient,
    uint256 amount
) external
```

Transfer `amount` tokens from the `sender` to `recipient`.
If `spender` isn't the same as `sender`, checks if `spender` has allowance to
spend tokens of `sender`.

Modifiers:
 - onlySelf

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `spender` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

### selfApproveFor

```solidity
function selfApproveFor(
    address account,
    address spender,
    uint256 amount
) external
```

Give `spender`, `amount` allowance to spend the tokens of
`account`.

Modifiers:
 - onlySelf

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `spender` | address |  |
| `amount` | uint256 |  |

### transferAll

```solidity
function transferAll(
    address recipient
) external
```

Transfer all available balance from `msg.sender` to `recipient`

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `recipient` | address |  |

### getUnderlyingToken

```solidity
function getUnderlyingToken(
) external returns (address tokenAddr)
```

Return the underlying token contract

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `tokenAddr` | address | Underlying token address |

### upgrade

```solidity
function upgrade(
    uint256 amount
) external
```

/**

Upgrade ERC20 to SuperToken.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 | Number of tokens to be upgraded (in 18 decimals)

NOTE: It will use ´transferFrom´ to get tokens. Before calling this
function you should ´approve´ this contract
/ |

### upgradeTo

```solidity
function upgradeTo(
    address to,
    uint256 amount,
    bytes data
) external
```

Upgrade ERC20 to SuperToken and transfer immediately

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address | The account to received upgraded tokens |
| `amount` | uint256 | Number of tokens to be upgraded (in 18 decimals) |
| `data` | bytes | User data for the TokensRecipient callback

NOTE: It will use ´transferFrom´ to get tokens. Before calling this
function you should ´approve´ this contract
/
    fun |

### downgrade

```solidity
function downgrade(
    uint256 amount
) external
```

Downgrade SuperToken to ERC20.
It will call transfer to send tokens

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 | Number of tokens to be downgraded
/
    fun |

### operationApprove

```solidity
function operationApprove(
    address account,
    address spender,
    uint256 amount
) external
```

Perform ERC20 approve by host contract.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account owner to be approved. |
| `spender` | address | The spender of account owner's funds. |
| `amount` | uint256 | Number of tokens to be approved.

Modifiers:
 - onlyHost
/
    fun |

### operationTransferFrom

```solidity
function operationTransferFrom(
    address account,
    address spender,
    address recipient,
    uint256 amount
) external
```

Perform ERC20 transfer from by host contract.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to spend sender's funds. |
| `spender` | address | The account where the funds is sent from. |
| `recipient` | address | The recipient of thefunds. |
| `amount` | uint256 | Number of tokens to be transferred.

Modifiers:
 - onlyHost
/
    fun |

### operationUpgrade

```solidity
function operationUpgrade(
    address account,
    uint256 amount
) external
```

Upgrade ERC20 to SuperToken by host contract.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to be changed. |
| `amount` | uint256 | Number of tokens to be upgraded (in 18 decimals)

Modifiers:
 - onlyHost
/
    fun |

### operationDowngrade

```solidity
function operationDowngrade(
    address account,
    uint256 amount
) external
```

Downgrade ERC20 to SuperToken by host contract.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | The account to be changed. |
| `amount` | uint256 | Number of tokens to be downgraded (in 18 decimals)

Modifiers:
 - onlyHost
/
    fun |

## Events

### TokenUpgraded

```solidity
event TokenUpgraded(
    address account,
    uint256 amount
)
```

Token upgrade event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | Account where tokens are upgraded to |
| `amount` | uint256 | Amount of tokens upgraded (in 18 decimals)
/
    eve |
### TokenDowngraded

```solidity
event TokenDowngraded(
    address account,
    uint256 amount
)
```

Token downgrade event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address | Account whose tokens are upgraded |
| `amount` | uint256 | Amount of tokens downgraded
/
    eve |

