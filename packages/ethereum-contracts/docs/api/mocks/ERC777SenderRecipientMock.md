# Solidity API

## ERC777SenderRecipientMock

### TokensToSendCalled

```solidity
event TokensToSendCalled(address operator, address from, address to, uint256 amount, bytes data, bytes operatorData, address token, uint256 fromBalance, uint256 toBalance)
```

### TokensReceivedCalled

```solidity
event TokensReceivedCalled(address operator, address from, address to, uint256 amount, bytes data, bytes operatorData, address token, uint256 fromBalance, uint256 toBalance)
```

### _shouldRevertSend

```solidity
bool _shouldRevertSend
```

### _shouldRevertReceive

```solidity
bool _shouldRevertReceive
```

### _erc1820

```solidity
contract IERC1820Registry _erc1820
```

### _TOKENS_SENDER_INTERFACE_HASH

```solidity
bytes32 _TOKENS_SENDER_INTERFACE_HASH
```

### _TOKENS_RECIPIENT_INTERFACE_HASH

```solidity
bytes32 _TOKENS_RECIPIENT_INTERFACE_HASH
```

### tokensToSend

```solidity
function tokensToSend(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData) external
```

_Called by an {IERC777} token contract whenever a registered holder&#x27;s
(&#x60;from&#x60;) tokens are about to be moved or destroyed. The type of operation
is conveyed by &#x60;to&#x60; being the zero address or not.

This call occurs _before_ the token contract&#x27;s state is updated, so
{IERC777-balanceOf}, etc., can be used to query the pre-operation state.

This function may revert to prevent the operation from being executed._

### tokensReceived

```solidity
function tokensReceived(address operator, address from, address to, uint256 amount, bytes userData, bytes operatorData) external
```

_Called by an {IERC777} token contract whenever tokens are being
moved or created into a registered account (&#x60;to&#x60;). The type of operation
is conveyed by &#x60;from&#x60; being the zero address or not.

This call occurs _after_ the token contract&#x27;s state is updated, so
{IERC777-balanceOf}, etc., can be used to query the post-operation state.

This function may revert to prevent the operation from being executed._

### senderFor

```solidity
function senderFor(address account) public
```

### registerSender

```solidity
function registerSender(address sender) public
```

### recipientFor

```solidity
function recipientFor(address account) public
```

### registerRecipient

```solidity
function registerRecipient(address recipient) public
```

### setShouldRevertSend

```solidity
function setShouldRevertSend(bool shouldRevert) public
```

### setShouldRevertReceive

```solidity
function setShouldRevertReceive(bool shouldRevert) public
```

### send

```solidity
function send(contract IERC777 token, address to, uint256 amount, bytes data) public
```

### burn

```solidity
function burn(contract IERC777 token, uint256 amount, bytes data) public
```

### upgradeAll

```solidity
function upgradeAll(contract ISuperToken token) public
```

### upgradeAllToSelf

```solidity
function upgradeAllToSelf(contract ISuperToken token) public
```

## ERC777RecipientReverting

### _TOKENS_RECIPIENT_INTERFACE_HASH

```solidity
bytes32 _TOKENS_RECIPIENT_INTERFACE_HASH
```

### constructor

```solidity
constructor() public
```

### tokensReceived

```solidity
function tokensReceived(address, address, address, uint256, bytes, bytes) external pure
```

### canImplementInterfaceForAddress

```solidity
function canImplementInterfaceForAddress(bytes32 interfaceHash, address) external pure returns (bytes32)
```

## ERC777RecipientDrainingGas

### _TOKENS_RECIPIENT_INTERFACE_HASH

```solidity
bytes32 _TOKENS_RECIPIENT_INTERFACE_HASH
```

### _uselessVar

```solidity
uint256 _uselessVar
```

### DrainedGas

```solidity
event DrainedGas(uint256 allowance, uint256 burned)
```

### tokensReceived

```solidity
function tokensReceived(address, address, address, uint256, bytes, bytes) external
```

### canImplementInterfaceForAddress

```solidity
function canImplementInterfaceForAddress(bytes32 interfaceHash, address) external pure returns (bytes32)
```

