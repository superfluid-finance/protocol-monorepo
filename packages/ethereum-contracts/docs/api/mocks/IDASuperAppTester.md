# IDASuperAppTester

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    uint256 configWord,
    contract IInstantDistributionAgreementV1 ida,
    contract ISuperToken token,
    uint32 indexId
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `configWord` | uint256 |  |
| `ida` | contract IInstantDistributionAgreementV1 |  |
| `token` | contract ISuperToken |  |
| `indexId` | uint32 |  |

### updateSubscription

```solidity
function updateSubscription(
    address subscriber,
    uint128 units
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `subscriber` | address |  |
| `units` | uint128 |  |

### distribute

```solidity
function distribute(
    uint128 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `amount` | uint128 |  |

### _expectCallback

```solidity
function _expectCallback(
    bytes ctx,
    bytes32 callbackType,
    bytes agreementData
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |
| `callbackType` | bytes32 |  |
| `agreementData` | bytes |  |

### _packSubscriptionData

```solidity
function _packSubscriptionData(
    bytes32 agreementId
) private returns (bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementId` | bytes32 |  |

### setForceGetSubscriptionByID

```solidity
function setForceGetSubscriptionByID(
) external
```

### _emitSubscriptionDataEvents

```solidity
function _emitSubscriptionDataEvents(
    bytes cbdata,
    bytes32 agreementId,
    bool deleted
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes |  |
| `agreementId` | bytes32 |  |
| `deleted` | bool |  |

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes cbdata)
```

Callback before a new agreement is created.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes | A free format in memory data the app can use to pass
         arbitary information to the after-hook callback.

NOTE:
- It will be invoked with `staticcall`, no state changes are permitted.
- Only revert with a "reason" is permitted. |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is created.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Only revert with a "reason" is permitted. |

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes)
```

Callback before a new agreement is updated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bytes |  |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is updated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Only revert with a "reason" is permitted. |

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes)
```

Callback before a new agreement is terminated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is terminated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Revert is not permitted. |

## Events

### SubscriptionDataBefore

```solidity
event SubscriptionDataBefore(
    address publisher,
    uint32 indexId,
    bool approved,
    uint128 units,
    uint256 pendingDistribution
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `approved` | bool |  |
| `units` | uint128 |  |
| `pendingDistribution` | uint256 |  |
### SubscriptionDataAfter

```solidity
event SubscriptionDataAfter(
    address publisher,
    uint32 indexId,
    bool approved,
    uint128 units,
    uint256 pendingDistribution
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `approved` | bool |  |
| `units` | uint128 |  |
| `pendingDistribution` | uint256 |  |

