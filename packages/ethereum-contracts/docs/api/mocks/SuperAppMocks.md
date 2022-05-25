# SuperAppMockAux

## Functions

### actionPingAgreement

```solidity
function actionPingAgreement(
    contract ISuperfluid host,
    contract AgreementMock agreement,
    uint256 ping,
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `agreement` | contract AgreementMock |  |
| `ping` | uint256 |  |
| `ctx` | bytes |  |

### actionCallActionNoop

```solidity
function actionCallActionNoop(
    contract ISuperfluid host,
    contract SuperAppMock app,
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `app` | contract SuperAppMock |  |
| `ctx` | bytes |  |

# SuperAppMock

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    uint256 configWord,
    bool doubleRegistration
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `configWord` | uint256 |  |
| `doubleRegistration` | bool |  |

### tryRegisterApp

```solidity
function tryRegisterApp(
    uint256 configWord
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 |  |

### allowCompositeApp

```solidity
function allowCompositeApp(
    contract ISuperApp target
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `target` | contract ISuperApp |  |

### actionNoop

```solidity
function actionNoop(
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionExpectMsgSender

```solidity
function actionExpectMsgSender(
    address expectedMsgSender,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `expectedMsgSender` | address |  |
| `ctx` | bytes |  |

### actionAssert

```solidity
function actionAssert(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionRevert

```solidity
function actionRevert(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionRevertWithReason

```solidity
function actionRevertWithReason(
    string reason,
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `reason` | string |  |
| `ctx` | bytes |  |

### actionCallAgreementWithoutCtx

```solidity
function actionCallAgreementWithoutCtx(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionCallAppActionWithoutCtx

```solidity
function actionCallAppActionWithoutCtx(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionAlteringCtx

```solidity
function actionAlteringCtx(
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionReturnEmptyCtx

```solidity
function actionReturnEmptyCtx(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionPingAgreementThroughAux

```solidity
function actionPingAgreementThroughAux(
    contract AgreementMock agreement,
    uint256 ping,
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreement` | contract AgreementMock |  |
| `ping` | uint256 |  |
| `ctx` | bytes |  |

### actionCallActionNoopThroughAux

```solidity
function actionCallActionNoopThroughAux(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionPingAgreement

```solidity
function actionPingAgreement(
    contract AgreementMock agreement,
    uint256 ping,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreement` | contract AgreementMock |  |
| `ping` | uint256 |  |
| `ctx` | bytes |  |

### actionAgreementRevert

```solidity
function actionAgreementRevert(
    contract AgreementMock agreement,
    string reason,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreement` | contract AgreementMock |  |
| `reason` | string |  |
| `ctx` | bytes |  |

### actionCallActionNoop

```solidity
function actionCallActionNoop(
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### actionCallActionRevert

```solidity
function actionCallActionRevert(
    string reason,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `reason` | string |  |
| `ctx` | bytes |  |

### actionCallAgreementWithInvalidCtx

```solidity
function actionCallAgreementWithInvalidCtx(
    contract AgreementMock agreement,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreement` | contract AgreementMock |  |
| `ctx` | bytes |  |

### actionCallActionWithInvalidCtx

```solidity
function actionCallActionWithInvalidCtx(
    string reason,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `reason` | string |  |
| `ctx` | bytes |  |

### actionCallBadAction

```solidity
function actionCallBadAction(
    bytes ctx
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### setNextCallbackAction

```solidity
function setNextCallbackAction(
    enum SuperAppMock.NextCallbackActionType actionType,
    bytes data
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `actionType` | enum SuperAppMock.NextCallbackActionType |  |
| `data` | bytes |  |

### _executeBeforeCallbackAction

```solidity
function _executeBeforeCallbackAction(
) private returns (bytes cbdata)
```

### _executeAfterCallbackAction

```solidity
function _executeAfterCallbackAction(
    bytes ctx
) private returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ctx
) external returns (bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ctx
) external returns (bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ctx
) external returns (bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### _burnGas

```solidity
function _burnGas(
    uint256 gasToBurn
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `gasToBurn` | uint256 |  |

## Events

### NoopEvent

```solidity
event NoopEvent(
    uint8 appLevel,
    uint8 callType,
    bytes4 agreementSelector
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appLevel` | uint8 |  |
| `callType` | uint8 |  |
| `agreementSelector` | bytes4 |  |

# SuperAppMockReturningEmptyCtx

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

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `` | bytes |  |

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes 
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `` | bytes |  |

# SuperAppMockReturningInvalidCtx

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

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes 
) external returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes 
) external returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `` | bytes |  |

# SuperAppMock2ndLevel

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    contract SuperAppMock app,
    contract AgreementMock agreement
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `app` | contract SuperAppMock |  |
| `agreement` | contract AgreementMock |  |

### allowCompositeApp

```solidity
function allowCompositeApp(
) external
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

# SuperAppMockWithRegistrationkey

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    uint256 configWord,
    string registrationKey
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `configWord` | uint256 |  |
| `registrationKey` | string |  |

# SuperAppMockNotSelfRegistering

# SuperAppFactoryMock

## Functions

### registerAppWithHost

```solidity
function registerAppWithHost(
    contract ISuperfluid host,
    contract ISuperApp app,
    uint256 configWord
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `app` | contract ISuperApp |  |
| `configWord` | uint256 |  |

