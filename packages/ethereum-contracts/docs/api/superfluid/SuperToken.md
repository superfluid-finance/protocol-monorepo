# Solidity API

## SuperToken

### _STANDARD_DECIMALS

```solidity
uint8 _STANDARD_DECIMALS
```

### _underlyingToken

```solidity
contract IERC20 _underlyingToken
```

_The underlying ERC20 token_

### _underlyingDecimals

```solidity
uint8 _underlyingDecimals
```

_Decimals of the underlying token_

### _name

```solidity
string _name
```

_TokenInfo Name property_

### _symbol

```solidity
string _symbol
```

_TokenInfo Symbol property_

### _allowances

```solidity
mapping(address &#x3D;&gt; mapping(address &#x3D;&gt; uint256)) _allowances
```

_ERC20 Allowances Storage_

### _operators

```solidity
struct ERC777Helper.Operators _operators
```

_ERC777 operators support data_

### _reserve22

```solidity
uint256 _reserve22
```

### _reserve23

```solidity
uint256 _reserve23
```

### _reserve24

```solidity
uint256 _reserve24
```

### _reserve25

```solidity
uint256 _reserve25
```

### _reserve26

```solidity
uint256 _reserve26
```

### _reserve27

```solidity
uint256 _reserve27
```

### _reserve28

```solidity
uint256 _reserve28
```

### _reserve29

```solidity
uint256 _reserve29
```

### _reserve30

```solidity
uint256 _reserve30
```

### _reserve31

```solidity
uint256 _reserve31
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### initialize

```solidity
function initialize(contract IERC20 underlyingToken, uint8 underlyingDecimals, string n, string s) external
```

_Initialize the contract_

### proxiableUUID

```solidity
function proxiableUUID() public pure returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### updateCode

```solidity
function updateCode(address newAddress) external
```

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
function decimals() external pure returns (uint8)
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

### _transferFrom

```solidity
function _transferFrom(address spender, address holder, address recipient, uint256 amount) internal returns (bool)
```

in the original openzeppelin implementation, transfer() and transferFrom()
did invoke the send and receive hooks, as required by ERC777.
This hooks were removed from super tokens for ERC20 transfers in order to protect
interfacing contracts which don&#x27;t expect invocations of ERC20 transfers to potentially reenter.
Interactions relying on ERC777 hooks need to use the ERC777 interface.
For more context, see https://github.com/superfluid-finance/protocol-monorepo/wiki/About-ERC-777

### _send

```solidity
function _send(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData, bool requireReceptionAck) private
```

_Send tokens_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operator | address | address operator address |
| from | address | address token holder address |
| to | address | address recipient address |
| amount | uint256 | uint256 amount of tokens to transfer |
| userData | bytes | bytes extra information provided by the token holder (if any) |
| operatorData | bytes | bytes extra information provided by the operator (if any) |
| requireReceptionAck | bool | if true, contract recipients are required to implement ERC777TokensRecipient |

### _move

```solidity
function _move(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData) private
```

### _mint

```solidity
function _mint(address operator, address account, uint256 amount, bool requireReceptionAck, bytes userData, bytes operatorData) internal
```

_Creates &#x60;amount&#x60; tokens and assigns them to &#x60;account&#x60;, increasing
the total supply.

If a send hook is registered for &#x60;account&#x60;, the corresponding function
will be called with &#x60;operator&#x60;, &#x60;data&#x60; and &#x60;operatorData&#x60;.

See {IERC777Sender} and {IERC777Recipient}.

Emits {Minted} and {IERC20-Transfer} events.

Requirements

- &#x60;account&#x60; cannot be the zero address.
- if &#x60;account&#x60; is a contract, it must implement the {IERC777Recipient}
interface._

### _burn

```solidity
function _burn(address operator, address from, uint256 amount, bytes userData, bytes operatorData) internal
```

_Burn tokens_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operator | address |  |
| from | address | address token holder address |
| amount | uint256 | uint256 amount of tokens to burn |
| userData | bytes | bytes extra information provided by the token holder |
| operatorData | bytes | bytes extra information provided by the operator (if any) |

### _approve

```solidity
function _approve(address account, address spender, uint256 amount) internal
```

Sets &#x60;amount&#x60; as the allowance of &#x60;spender&#x60; over the &#x60;account&#x60;s tokens.

This is internal function is equivalent to &#x60;approve&#x60;, and can be used to
e.g. set automatic allowances for certain subsystems, etc.

Emits an {Approval} event.

Requirements:

- &#x60;account&#x60; cannot be the zero address.
- &#x60;spender&#x60; cannot be the zero address.

### _callTokensToSend

```solidity
function _callTokensToSend(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData) private
```

_Call from.tokensToSend() if the interface is registered_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operator | address | address operator requesting the transfer |
| from | address | address token holder address |
| to | address | address recipient address |
| amount | uint256 | uint256 amount of tokens to transfer |
| userData | bytes | bytes extra information provided by the token holder (if any) |
| operatorData | bytes | bytes extra information provided by the operator (if any) |

### _callTokensReceived

```solidity
function _callTokensReceived(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData, bool requireReceptionAck) private
```

_Call to.tokensReceived() if the interface is registered. Reverts if the recipient is a contract but
tokensReceived() was not registered for the recipient_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operator | address | address operator requesting the transfer |
| from | address | address token holder address |
| to | address | address recipient address |
| amount | uint256 | uint256 amount of tokens to transfer |
| userData | bytes | bytes extra information provided by the token holder (if any) |
| operatorData | bytes | bytes extra information provided by the operator (if any) |
| requireReceptionAck | bool | if true, contract recipients are required to implement ERC777TokensRecipient |

### totalSupply

```solidity
function totalSupply() public view returns (uint256)
```

_See {IERC20-totalSupply}._

### balanceOf

```solidity
function balanceOf(address account) public view returns (uint256 balance)
```

_Returns the amount of tokens owned by an account (&#x60;owner&#x60;)._

### transfer

```solidity
function transfer(address recipient, uint256 amount) public returns (bool)
```

_Moves &#x60;amount&#x60; tokens from the caller&#x27;s account to &#x60;recipient&#x60;._

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | Returns Success a boolean value indicating whether the operation succeeded. Emits a {Transfer} event. |

### allowance

```solidity
function allowance(address account, address spender) public view returns (uint256)
```

### approve

```solidity
function approve(address spender, uint256 amount) public returns (bool)
```

_Sets &#x60;amount&#x60; as the allowance of &#x60;spender&#x60; over the caller&#x27;s tokens._

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | Returns Success a boolean value indicating whether the operation succeeded. IMPORTANT: Beware that changing an allowance with this method brings the risk that someone may use both the old and the new allowance by unfortunate transaction ordering. One possible solution to mitigate this race condition is to first reduce the spender&#x27;s allowance to 0 and set the desired value afterwards: https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729 Emits an {Approval} event. |

### transferFrom

```solidity
function transferFrom(address holder, address recipient, uint256 amount) public returns (bool)
```

### increaseAllowance

```solidity
function increaseAllowance(address spender, uint256 addedValue) public returns (bool)
```

_Atomically increases the allowance granted to &#x60;spender&#x60; by the caller.

This is an alternative to {approve} that can be used as a mitigation for
problems described in {IERC20-approve}.

Emits an {Approval} event indicating the updated allowance.

Requirements:

- &#x60;spender&#x60; cannot be the zero address._

### decreaseAllowance

```solidity
function decreaseAllowance(address spender, uint256 subtractedValue) public returns (bool)
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
function granularity() external pure returns (uint256)
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

### _setupDefaultOperators

```solidity
function _setupDefaultOperators(address[] operators) internal
```

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

### selfApproveFor

```solidity
function selfApproveFor(address account, address spender, uint256 amount) external
```

_Give &#x60;spender&#x60;, &#x60;amount&#x60; allowance to spend the tokens of
&#x60;account&#x60;.

Modifiers:
 - onlySelf_

### selfTransferFrom

```solidity
function selfTransferFrom(address holder, address spender, address recipient, uint256 amount) external
```

### transferAll

```solidity
function transferAll(address recipient) external
```

_Transfer all available balance from &#x60;msg.sender&#x60; to &#x60;recipient&#x60;_

### getUnderlyingToken

```solidity
function getUnderlyingToken() external view returns (address)
```

_ISuperfluidGovernance.getUnderlyingToken implementation_

### upgrade

```solidity
function upgrade(uint256 amount) external
```

_ISuperToken.upgrade implementation_

### upgradeTo

```solidity
function upgradeTo(address to, uint256 amount, bytes data) external
```

_ISuperToken.upgradeTo implementation_

### downgrade

```solidity
function downgrade(uint256 amount) external
```

_ISuperToken.downgrade implementation_

### _upgrade

```solidity
function _upgrade(address operator, address account, address to, uint256 amount, bytes userData, bytes operatorData) private
```

### _downgrade

```solidity
function _downgrade(address operator, address account, uint256 amount, bytes data, bytes operatorData) private
```

### _toUnderlyingAmount

```solidity
function _toUnderlyingAmount(uint256 amount) private view returns (uint256 underlyingAmount, uint256 adjustedAmount)
```

_Handle decimal differences between underlying token and super token_

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

### onlySelf

```solidity
modifier onlySelf()
```

