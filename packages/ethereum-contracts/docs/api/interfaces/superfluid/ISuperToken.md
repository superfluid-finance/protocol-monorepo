# Solidity API

## ISuperToken

### initialize

```solidity
function initialize(contract IERC20 underlyingToken, uint8 underlyingDecimals, string n, string s) external
```

_Initialize the contract_

### name

```solidity
function name() external view returns (string)
```

_Returns the name of the token._

### symbol

```solidity
function symbol() external view returns (string)
```

_Returns the symbol of the token, usually a shorter version of the
name._

### decimals

```solidity
function decimals() external view returns (uint8)
```

_Returns the number of decimals used to get its user representation.
For example, if &#x60;decimals&#x60; equals &#x60;2&#x60;, a balance of &#x60;505&#x60; tokens should
be displayed to a user as &#x60;5,05&#x60; (&#x60;505 / 10 ** 2&#x60;).

Tokens usually opt for a value of 18, imitating the relationship between
Ether and Wei. This is the value {ERC20} uses, unless {_setupDecimals} is
called.

NOTE: SuperToken always uses 18 decimals.

Note: This information is only used for _display_ purposes: it in
no way affects any of the arithmetic of the contract, including
{IERC20-balanceOf} and {IERC20-transfer}._

### totalSupply

```solidity
function totalSupply() external view returns (uint256)
```

_See {IERC20-totalSupply}._

### balanceOf

```solidity
function balanceOf(address account) external view returns (uint256 balance)
```

_Returns the amount of tokens owned by an account (&#x60;owner&#x60;)._

### transfer

```solidity
function transfer(address recipient, uint256 amount) external returns (bool)
```

_Moves &#x60;amount&#x60; tokens from the caller&#x27;s account to &#x60;recipient&#x60;._

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | Returns Success a boolean value indicating whether the operation succeeded. Emits a {Transfer} event. |

### allowance

```solidity
function allowance(address owner, address spender) external view returns (uint256)
```

_Returns the remaining number of tokens that &#x60;spender&#x60; will be
        allowed to spend on behalf of &#x60;owner&#x60; through {transferFrom}. This is
        zero by default.

This value changes when {approve} or {transferFrom} are called._

### approve

```solidity
function approve(address spender, uint256 amount) external returns (bool)
```

_Sets &#x60;amount&#x60; as the allowance of &#x60;spender&#x60; over the caller&#x27;s tokens._

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | Returns Success a boolean value indicating whether the operation succeeded. IMPORTANT: Beware that changing an allowance with this method brings the risk that someone may use both the old and the new allowance by unfortunate transaction ordering. One possible solution to mitigate this race condition is to first reduce the spender&#x27;s allowance to 0 and set the desired value afterwards: https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729 Emits an {Approval} event. |

### transferFrom

```solidity
function transferFrom(address sender, address recipient, uint256 amount) external returns (bool)
```

_Moves &#x60;amount&#x60; tokens from &#x60;sender&#x60; to &#x60;recipient&#x60; using the
        allowance mechanism. &#x60;amount&#x60; is then deducted from the caller&#x27;s
        allowance._

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | Returns Success a boolean value indicating whether the operation succeeded. Emits a {Transfer} event. |

### increaseAllowance

```solidity
function increaseAllowance(address spender, uint256 addedValue) external returns (bool)
```

_Atomically increases the allowance granted to &#x60;spender&#x60; by the caller.

This is an alternative to {approve} that can be used as a mitigation for
problems described in {IERC20-approve}.

Emits an {Approval} event indicating the updated allowance.

Requirements:

- &#x60;spender&#x60; cannot be the zero address._

### decreaseAllowance

```solidity
function decreaseAllowance(address spender, uint256 subtractedValue) external returns (bool)
```

_Atomically decreases the allowance granted to &#x60;spender&#x60; by the caller.

This is an alternative to {approve} that can be used as a mitigation for
problems described in {IERC20-approve}.

Emits an {Approval} event indicating the updated allowance.

Requirements:

- &#x60;spender&#x60; cannot be the zero address.
- &#x60;spender&#x60; must have allowance for the caller of at least
&#x60;subtractedValue&#x60;._

### granularity

```solidity
function granularity() external view returns (uint256)
```

_Returns the smallest part of the token that is not divisible. This
        means all token operations (creation, movement and destruction) must have
        amounts that are a multiple of this number.

For super token contracts, this value is 1 always_

### send

```solidity
function send(address recipient, uint256 amount, bytes data) external
```

_Moves &#x60;amount&#x60; tokens from the caller&#x27;s account to &#x60;recipient&#x60;.

If send or receive hooks are registered for the caller and &#x60;recipient&#x60;,
     the corresponding functions will be called with &#x60;data&#x60; and empty
     &#x60;operatorData&#x60;. See {IERC777Sender} and {IERC777Recipient}.

Emits a {Sent} event.

Requirements

- the caller must have at least &#x60;amount&#x60; tokens.
- &#x60;recipient&#x60; cannot be the zero address.
- if &#x60;recipient&#x60; is a contract, it must implement the {IERC777Recipient}
interface._

### burn

```solidity
function burn(uint256 amount, bytes data) external
```

_Destroys &#x60;amount&#x60; tokens from the caller&#x27;s account, reducing the
total supply.

If a send hook is registered for the caller, the corresponding function
will be called with &#x60;data&#x60; and empty &#x60;operatorData&#x60;. See {IERC777Sender}.

Emits a {Burned} event.

Requirements

- the caller must have at least &#x60;amount&#x60; tokens._

### isOperatorFor

```solidity
function isOperatorFor(address operator, address tokenHolder) external view returns (bool)
```

_Returns true if an account is an operator of &#x60;tokenHolder&#x60;.
Operators can send and burn tokens on behalf of their owners. All
accounts are their own operator.

See {operatorSend} and {operatorBurn}._

### authorizeOperator

```solidity
function authorizeOperator(address operator) external
```

_Make an account an operator of the caller.

See {isOperatorFor}.

Emits an {AuthorizedOperator} event.

Requirements

- &#x60;operator&#x60; cannot be calling address._

### revokeOperator

```solidity
function revokeOperator(address operator) external
```

_Revoke an account&#x27;s operator status for the caller.

See {isOperatorFor} and {defaultOperators}.

Emits a {RevokedOperator} event.

Requirements

- &#x60;operator&#x60; cannot be calling address._

### defaultOperators

```solidity
function defaultOperators() external view returns (address[])
```

_Returns the list of default operators. These accounts are operators
for all token holders, even if {authorizeOperator} was never called on
them.

This list is immutable, but individual holders may revoke these via
{revokeOperator}, in which case {isOperatorFor} will return false._

### operatorSend

```solidity
function operatorSend(address sender, address recipient, uint256 amount, bytes data, bytes operatorData) external
```

_Moves &#x60;amount&#x60; tokens from &#x60;sender&#x60; to &#x60;recipient&#x60;. The caller must
be an operator of &#x60;sender&#x60;.

If send or receive hooks are registered for &#x60;sender&#x60; and &#x60;recipient&#x60;,
the corresponding functions will be called with &#x60;data&#x60; and
&#x60;operatorData&#x60;. See {IERC777Sender} and {IERC777Recipient}.

Emits a {Sent} event.

Requirements

- &#x60;sender&#x60; cannot be the zero address.
- &#x60;sender&#x60; must have at least &#x60;amount&#x60; tokens.
- the caller must be an operator for &#x60;sender&#x60;.
- &#x60;recipient&#x60; cannot be the zero address.
- if &#x60;recipient&#x60; is a contract, it must implement the {IERC777Recipient}
interface._

### operatorBurn

```solidity
function operatorBurn(address account, uint256 amount, bytes data, bytes operatorData) external
```

_Destroys &#x60;amount&#x60; tokens from &#x60;account&#x60;, reducing the total supply.
The caller must be an operator of &#x60;account&#x60;.

If a send hook is registered for &#x60;account&#x60;, the corresponding function
will be called with &#x60;data&#x60; and &#x60;operatorData&#x60;. See {IERC777Sender}.

Emits a {Burned} event.

Requirements

- &#x60;account&#x60; cannot be the zero address.
- &#x60;account&#x60; must have at least &#x60;amount&#x60; tokens.
- the caller must be an operator for &#x60;account&#x60;._

### selfMint

```solidity
function selfMint(address account, uint256 amount, bytes userData) external
```

_Mint new tokens for the account

Modifiers:
 - onlySelf_

### selfBurn

```solidity
function selfBurn(address account, uint256 amount, bytes userData) external
```

_Burn existing tokens for the account

Modifiers:
 - onlySelf_

### selfTransferFrom

```solidity
function selfTransferFrom(address sender, address spender, address recipient, uint256 amount) external
```

_Transfer &#x60;amount&#x60; tokens from the &#x60;sender&#x60; to &#x60;recipient&#x60;.
If &#x60;spender&#x60; isn&#x27;t the same as &#x60;sender&#x60;, checks if &#x60;spender&#x60; has allowance to
spend tokens of &#x60;sender&#x60;.

Modifiers:
 - onlySelf_

### selfApproveFor

```solidity
function selfApproveFor(address account, address spender, uint256 amount) external
```

_Give &#x60;spender&#x60;, &#x60;amount&#x60; allowance to spend the tokens of
&#x60;account&#x60;.

Modifiers:
 - onlySelf_

### transferAll

```solidity
function transferAll(address recipient) external
```

_Transfer all available balance from &#x60;msg.sender&#x60; to &#x60;recipient&#x60;_

### getUnderlyingToken

```solidity
function getUnderlyingToken() external view returns (address tokenAddr)
```

_Return the underlying token contract_

| Name | Type | Description |
| ---- | ---- | ----------- |
| tokenAddr | address | Underlying token address |

### upgrade

```solidity
function upgrade(uint256 amount) external
```

/**

_Upgrade ERC20 to SuperToken._

| Name | Type | Description |
| ---- | ---- | ----------- |
| amount | uint256 | Number of tokens to be upgraded (in 18 decimals) NOTE: It will use ´transferFrom´ to get tokens. Before calling this function you should ´approve´ this contract / |

### upgradeTo

```solidity
function upgradeTo(address to, uint256 amount, bytes data) external
```

_Upgrade ERC20 to SuperToken and transfer immediately_

| Name | Type | Description |
| ---- | ---- | ----------- |
| to | address | The account to received upgraded tokens |
| amount | uint256 | Number of tokens to be upgraded (in 18 decimals) |
| data | bytes | User data for the TokensRecipient callback NOTE: It will use ´transferFrom´ to get tokens. Before calling this function you should ´approve´ this contract /     fun |

### TokenUpgraded

```solidity
event TokenUpgraded(address account, uint256 amount)
```

_Token upgrade event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | Account where tokens are upgraded to |
| amount | uint256 | Amount of tokens upgraded (in 18 decimals) /     eve |

### downgrade

```solidity
function downgrade(uint256 amount) external
```

_Downgrade SuperToken to ERC20.
It will call transfer to send tokens_

| Name | Type | Description |
| ---- | ---- | ----------- |
| amount | uint256 | Number of tokens to be downgraded /     fun |

### TokenDowngraded

```solidity
event TokenDowngraded(address account, uint256 amount)
```

_Token downgrade event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | Account whose tokens are upgraded |
| amount | uint256 | Amount of tokens downgraded /     eve |

### operationApprove

```solidity
function operationApprove(address account, address spender, uint256 amount) external
```

_Perform ERC20 approve by host contract._

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account owner to be approved. |
| spender | address | The spender of account owner&#x27;s funds. |
| amount | uint256 | Number of tokens to be approved. Modifiers:  - onlyHost /     fun |

### operationTransferFrom

```solidity
function operationTransferFrom(address account, address spender, address recipient, uint256 amount) external
```

_Perform ERC20 transfer from by host contract._

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to spend sender&#x27;s funds. |
| spender | address | The account where the funds is sent from. |
| recipient | address | The recipient of thefunds. |
| amount | uint256 | Number of tokens to be transferred. Modifiers:  - onlyHost /     fun |

### operationUpgrade

```solidity
function operationUpgrade(address account, uint256 amount) external
```

_Upgrade ERC20 to SuperToken by host contract._

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to be changed. |
| amount | uint256 | Number of tokens to be upgraded (in 18 decimals) Modifiers:  - onlyHost /     fun |

### operationDowngrade

```solidity
function operationDowngrade(address account, uint256 amount) external
```

_Downgrade ERC20 to SuperToken by host contract._

| Name | Type | Description |
| ---- | ---- | ----------- |
| account | address | The account to be changed. |
| amount | uint256 | Number of tokens to be downgraded (in 18 decimals) Modifiers:  - onlyHost /     fun |

