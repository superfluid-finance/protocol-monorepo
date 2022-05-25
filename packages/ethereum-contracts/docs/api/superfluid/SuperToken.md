# SuperToken

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

### _transferFrom

```solidity
function _transferFrom(
    address spender,
    address holder,
    address recipient,
    uint256 amount
) internal returns (bool)
```

in the original openzeppelin implementation, transfer() and transferFrom()
did invoke the send and receive hooks, as required by ERC777.
This hooks were removed from super tokens for ERC20 transfers in order to protect
interfacing contracts which don't expect invocations of ERC20 transfers to potentially reenter.
Interactions relying on ERC777 hooks need to use the ERC777 interface.
For more context, see https://github.com/superfluid-finance/protocol-monorepo/wiki/About-ERC-777

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `spender` | address |  |
| `holder` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

### _send

```solidity
function _send(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData,
    bool requireReceptionAck
) private
```

Send tokens

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address | address operator address |
| `from` | address | address token holder address |
| `to` | address | address recipient address |
| `amount` | uint256 | uint256 amount of tokens to transfer |
| `userData` | bytes | bytes extra information provided by the token holder (if any) |
| `operatorData` | bytes | bytes extra information provided by the operator (if any) |
| `requireReceptionAck` | bool | if true, contract recipients are required to implement ERC777TokensRecipient |

### _move

```solidity
function _move(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

### _mint

```solidity
function _mint(
    address operator,
    address account,
    uint256 amount,
    bool requireReceptionAck,
    bytes userData,
    bytes operatorData
) internal
```

Creates `amount` tokens and assigns them to `account`, increasing
the total supply.

If a send hook is registered for `account`, the corresponding function
will be called with `operator`, `data` and `operatorData`.

See {IERC777Sender} and {IERC777Recipient}.

Emits {Minted} and {IERC20-Transfer} events.

Requirements

- `account` cannot be the zero address.
- if `account` is a contract, it must implement the {IERC777Recipient}
interface.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `account` | address |  |
| `amount` | uint256 |  |
| `requireReceptionAck` | bool |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

### _burn

```solidity
function _burn(
    address operator,
    address from,
    uint256 amount,
    bytes userData,
    bytes operatorData
) internal
```

Burn tokens

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address | address token holder address |
| `amount` | uint256 | uint256 amount of tokens to burn |
| `userData` | bytes | bytes extra information provided by the token holder |
| `operatorData` | bytes | bytes extra information provided by the operator (if any) |

### _approve

```solidity
function _approve(
    address account,
    address spender,
    uint256 amount
) internal
```

Sets `amount` as the allowance of `spender` over the `account`s tokens.

This is internal function is equivalent to `approve`, and can be used to
e.g. set automatic allowances for certain subsystems, etc.

Emits an {Approval} event.

Requirements:

- `account` cannot be the zero address.
- `spender` cannot be the zero address.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `spender` | address |  |
| `amount` | uint256 |  |

### _callTokensToSend

```solidity
function _callTokensToSend(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) private
```

Call from.tokensToSend() if the interface is registered

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address | address operator requesting the transfer |
| `from` | address | address token holder address |
| `to` | address | address recipient address |
| `amount` | uint256 | uint256 amount of tokens to transfer |
| `userData` | bytes | bytes extra information provided by the token holder (if any) |
| `operatorData` | bytes | bytes extra information provided by the operator (if any) |

### _callTokensReceived

```solidity
function _callTokensReceived(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData,
    bool requireReceptionAck
) private
```

Call to.tokensReceived() if the interface is registered. Reverts if the recipient is a contract but
tokensReceived() was not registered for the recipient

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address | address operator requesting the transfer |
| `from` | address | address token holder address |
| `to` | address | address recipient address |
| `amount` | uint256 | uint256 amount of tokens to transfer |
| `userData` | bytes | bytes extra information provided by the token holder (if any) |
| `operatorData` | bytes | bytes extra information provided by the operator (if any) |
| `requireReceptionAck` | bool | if true, contract recipients are required to implement ERC777TokensRecipient |

### totalSupply

```solidity
function totalSupply(
) public returns (uint256)
```

See {IERC20-totalSupply}.

### balanceOf

```solidity
function balanceOf(
    address account
) public returns (uint256 balance)
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
) public returns (bool)
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
    address account,
    address spender
) public returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |
| `spender` | address |  |

### approve

```solidity
function approve(
    address spender,
    uint256 amount
) public returns (bool)
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
    address holder,
    address recipient,
    uint256 amount
) public returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `holder` | address |  |
| `recipient` | address |  |
| `amount` | uint256 |  |

### increaseAllowance

```solidity
function increaseAllowance(
    address spender,
    uint256 addedValue
) public returns (bool)
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
) public returns (bool)
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

### _setupDefaultOperators

```solidity
function _setupDefaultOperators(
    address[] operators
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operators` | address[] |  |

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

### selfTransferFrom

```solidity
function selfTransferFrom(
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
) external returns (address)
```

ISuperfluidGovernance.getUnderlyingToken implementation

### upgrade

```solidity
function upgrade(
    uint256 amount
) external
```

ISuperToken.upgrade implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 |  |

### upgradeTo

```solidity
function upgradeTo(
    address to,
    uint256 amount,
    bytes data
) external
```

ISuperToken.upgradeTo implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `to` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |

### downgrade

```solidity
function downgrade(
    uint256 amount
) external
```

ISuperToken.downgrade implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 |  |

### _upgrade

```solidity
function _upgrade(
    address operator,
    address account,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `account` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

### _downgrade

```solidity
function _downgrade(
    address operator,
    address account,
    uint256 amount,
    bytes data,
    bytes operatorData
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `account` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |
| `operatorData` | bytes |  |

### _toUnderlyingAmount

```solidity
function _toUnderlyingAmount(
    uint256 amount
) private returns (uint256 underlyingAmount, uint256 adjustedAmount)
```

Handle decimal differences between underlying token and super token

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint256 |  |

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

