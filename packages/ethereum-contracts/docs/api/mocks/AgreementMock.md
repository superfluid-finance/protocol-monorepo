# AgreementMock

## Functions

### constructor

```solidity
function constructor(
    address host,
    bytes32 t,
    uint256 v
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | address |  |
| `t` | bytes32 |  |
| `v` | uint256 |  |

### version

```solidity
function version(
) external returns (uint256)
```

### agreementType

```solidity
function agreementType(
) external returns (bytes32)
```

Get the type of the agreement class

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(
    contract ISuperfluidToken token,
    address account,
    uint256 
) external returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `` | uint256 |  |

### setRealtimeBalanceFor

```solidity
function setRealtimeBalanceFor(
    contract ISuperfluidToken token,
    address account,
    int256 dynamicBalance,
    uint256 deposit,
    uint256 owedDeposit
) external
```

SuperfluidToken Mockings

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `dynamicBalance` | int256 |  |
| `deposit` | uint256 |  |
| `owedDeposit` | uint256 |  |

### createAgreementFor

```solidity
function createAgreementFor(
    contract ISuperfluidToken token,
    bytes32 id,
    bytes32[] data
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `id` | bytes32 |  |
| `data` | bytes32[] |  |

### updateAgreementDataFor

```solidity
function updateAgreementDataFor(
    contract ISuperfluidToken token,
    bytes32 id,
    bytes32[] data
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `id` | bytes32 |  |
| `data` | bytes32[] |  |

### terminateAgreementFor

```solidity
function terminateAgreementFor(
    contract ISuperfluidToken token,
    bytes32 id,
    uint256 dataLength
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `id` | bytes32 |  |
| `dataLength` | uint256 |  |

### updateAgreementStateSlotFor

```solidity
function updateAgreementStateSlotFor(
    contract ISuperfluidToken token,
    address account,
    uint256 slotId,
    bytes32[] slotData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `slotId` | uint256 |  |
| `slotData` | bytes32[] |  |

### settleBalanceFor

```solidity
function settleBalanceFor(
    contract ISuperfluidToken token,
    address account,
    int256 delta
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `delta` | int256 |  |

### makeLiquidationPayoutsFor

```solidity
function makeLiquidationPayoutsFor(
    contract ISuperfluidToken token,
    bytes32 id,
    bool useDefaultRewardAccount,
    address liquidator,
    address targetAccount,
    uint256 rewardAmount,
    int256 targetAccountBalanceDelta
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `id` | bytes32 |  |
| `useDefaultRewardAccount` | bool |  |
| `liquidator` | address |  |
| `targetAccount` | address |  |
| `rewardAmount` | uint256 |  |
| `targetAccountBalanceDelta` | int256 |  |

### tryCallAppBeforeCallback

```solidity
function tryCallAppBeforeCallback(
    contract ISuperfluid host,
    contract ISuperApp appMock,
    bool hackCtx,
    bytes ctx
) external returns (bytes newCtx)
```

Agreement Framework Mockings

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `appMock` | contract ISuperApp |  |
| `hackCtx` | bool |  |
| `ctx` | bytes |  |

### tryCallAppAfterCallback

```solidity
function tryCallAppAfterCallback(
    contract ISuperfluid host,
    contract ISuperApp appMock,
    bool hackCtx,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `appMock` | contract ISuperApp |  |
| `hackCtx` | bool |  |
| `ctx` | bytes |  |

### tryAppCallbackPush

```solidity
function tryAppCallbackPush(
    contract ISuperfluid host,
    contract ISuperApp appMock,
    bool hackCtx,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `appMock` | contract ISuperApp |  |
| `hackCtx` | bool |  |
| `ctx` | bytes |  |

### tryAppCallbackPop

```solidity
function tryAppCallbackPop(
    contract ISuperfluid host,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `ctx` | bytes |  |

### tryCtxUseAllowance

```solidity
function tryCtxUseAllowance(
    contract ISuperfluid host,
    bool hackCtx,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `hackCtx` | bool |  |
| `ctx` | bytes |  |

### tryJailApp

```solidity
function tryJailApp(
    contract ISuperfluid host,
    contract ISuperApp appMock,
    bool hackCtx,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `appMock` | contract ISuperApp |  |
| `hackCtx` | bool |  |
| `ctx` | bytes |  |

### doRevert

```solidity
function doRevert(
    string reason,
    bytes ctx
) external
```

doRevert agreement operation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `reason` | string |  |
| `ctx` | bytes |  |

### pingMe

```solidity
function pingMe(
    address expectedMsgSender,
    uint256 ping,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `expectedMsgSender` | address |  |
| `ping` | uint256 |  |
| `ctx` | bytes |  |

### _callAppBeforeCallback

```solidity
function _callAppBeforeCallback(
    contract ISuperApp app,
    uint256 noopBit,
    bytes ctx
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `noopBit` | uint256 |  |
| `ctx` | bytes |  |

### _callAppAfterAgreementCallback

```solidity
function _callAppAfterAgreementCallback(
    contract ISuperApp app,
    uint256 noopBit,
    bytes ctx
) private returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `noopBit` | uint256 |  |
| `ctx` | bytes |  |

### callAppBeforeAgreementCreatedCallback

```solidity
function callAppBeforeAgreementCreatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

### callAppAfterAgreementCreatedCallback

```solidity
function callAppAfterAgreementCreatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

### callAppBeforeAgreementUpdatedCallback

```solidity
function callAppBeforeAgreementUpdatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

### callAppAfterAgreementUpdatedCallback

```solidity
function callAppAfterAgreementUpdatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

### callAppBeforeAgreementTerminatedCallback

```solidity
function callAppBeforeAgreementTerminatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

### callAppAfterAgreementTerminatedCallback

```solidity
function callAppAfterAgreementTerminatedCallback(
    contract ISuperApp app,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `ctx` | bytes |  |

## Events

### Pong

```solidity
event Pong(
    uint256 ping
)
```

pingMe agreement operation, emits Pong event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ping` | uint256 |  |
### AppBeforeCallbackResult

```solidity
event AppBeforeCallbackResult(
    uint8 appLevel,
    uint8 callType,
    bytes4 agreementSelector,
    bytes cbdata
)
```

_callAppBeforeCallback base agreement operation, emits AppBeforeCallbackResult event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appLevel` | uint8 |  |
| `callType` | uint8 |  |
| `agreementSelector` | bytes4 |  |
| `cbdata` | bytes |  |
### AppAfterCallbackResult

```solidity
event AppAfterCallbackResult(
    uint8 appLevel,
    uint8 callType,
    bytes4 agreementSelector
)
```

_callAppAfterAgreementCallback base agreement operation, emits AppAfterCallbackResult event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appLevel` | uint8 |  |
| `callType` | uint8 |  |
| `agreementSelector` | bytes4 |  |

