# ERC777SenderRecipientMock

## Functions

### tokensToSend

```solidity
function tokensToSend(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) external
```

Called by an {IERC777} token contract whenever a registered holder's
(`from`) tokens are about to be moved or destroyed. The type of operation
is conveyed by `to` being the zero address or not.

This call occurs _before_ the token contract's state is updated, so
{IERC777-balanceOf}, etc., can be used to query the pre-operation state.

This function may revert to prevent the operation from being executed.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

### tokensReceived

```solidity
function tokensReceived(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes userData,
    bytes operatorData
) external
```

Called by an {IERC777} token contract whenever tokens are being
moved or created into a registered account (`to`). The type of operation
is conveyed by `from` being the zero address or not.

This call occurs _after_ the token contract's state is updated, so
{IERC777-balanceOf}, etc., can be used to query the post-operation state.

This function may revert to prevent the operation from being executed.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |
| `operatorData` | bytes |  |

### senderFor

```solidity
function senderFor(
    address account
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### registerSender

```solidity
function registerSender(
    address sender
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |

### recipientFor

```solidity
function recipientFor(
    address account
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `account` | address |  |

### registerRecipient

```solidity
function registerRecipient(
    address recipient
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `recipient` | address |  |

### setShouldRevertSend

```solidity
function setShouldRevertSend(
    bool shouldRevert
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `shouldRevert` | bool |  |

### setShouldRevertReceive

```solidity
function setShouldRevertReceive(
    bool shouldRevert
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `shouldRevert` | bool |  |

### send

```solidity
function send(
    contract IERC777 token,
    address to,
    uint256 amount,
    bytes data
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract IERC777 |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |

### burn

```solidity
function burn(
    contract IERC777 token,
    uint256 amount,
    bytes data
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract IERC777 |  |
| `amount` | uint256 |  |
| `data` | bytes |  |

### upgradeAll

```solidity
function upgradeAll(
    contract ISuperToken token
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

### upgradeAllToSelf

```solidity
function upgradeAllToSelf(
    contract ISuperToken token
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

## Events

### TokensToSendCalled

```solidity
event TokensToSendCalled(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes data,
    bytes operatorData,
    address token,
    uint256 fromBalance,
    uint256 toBalance
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |
| `operatorData` | bytes |  |
| `token` | address |  |
| `fromBalance` | uint256 |  |
| `toBalance` | uint256 |  |
### TokensReceivedCalled

```solidity
event TokensReceivedCalled(
    address operator,
    address from,
    address to,
    uint256 amount,
    bytes data,
    bytes operatorData,
    address token,
    uint256 fromBalance,
    uint256 toBalance
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operator` | address |  |
| `from` | address |  |
| `to` | address |  |
| `amount` | uint256 |  |
| `data` | bytes |  |
| `operatorData` | bytes |  |
| `token` | address |  |
| `fromBalance` | uint256 |  |
| `toBalance` | uint256 |  |

# ERC777RecipientReverting

## Functions

### constructor

```solidity
function constructor(
) public
```

### tokensReceived

```solidity
function tokensReceived(
    address ,
    address ,
    address ,
    uint256 ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | address |  |
| `` | address |  |
| `` | address |  |
| `` | uint256 |  |
| `` | bytes |  |
| `` | bytes |  |

### canImplementInterfaceForAddress

```solidity
function canImplementInterfaceForAddress(
    bytes32 interfaceHash,
    address 
) external returns (bytes32)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `interfaceHash` | bytes32 |  |
| `` | address |  |

# ERC777RecipientDrainingGas

## Functions

### tokensReceived

```solidity
function tokensReceived(
    address ,
    address ,
    address ,
    uint256 ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | address |  |
| `` | address |  |
| `` | address |  |
| `` | uint256 |  |
| `` | bytes |  |
| `` | bytes |  |

### canImplementInterfaceForAddress

```solidity
function canImplementInterfaceForAddress(
    bytes32 interfaceHash,
    address 
) external returns (bytes32)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `interfaceHash` | bytes32 |  |
| `` | address |  |

## Events

### DrainedGas

```solidity
event DrainedGas(
    uint256 allowance,
    uint256 burned
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `allowance` | uint256 |  |
| `burned` | uint256 |  |

