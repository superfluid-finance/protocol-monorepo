# Solidity API

## AgreementMock

### _REAL_TIME_BALANCE_SLOT_ID

```solidity
uint256 _REAL_TIME_BALANCE_SLOT_ID
```

### _type

```solidity
bytes32 _type
```

### _version

```solidity
uint256 _version
```

### constructor

```solidity
constructor(address host, bytes32 t, uint256 v) public
```

### version

```solidity
function version() external view returns (uint256)
```

### agreementType

```solidity
function agreementType() external view returns (bytes32)
```

_Get the type of the agreement class_

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(contract ISuperfluidToken token, address account, uint256) external view returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

### setRealtimeBalanceFor

```solidity
function setRealtimeBalanceFor(contract ISuperfluidToken token, address account, int256 dynamicBalance, uint256 deposit, uint256 owedDeposit) external
```

SuperfluidToken Mockings

### createAgreementFor

```solidity
function createAgreementFor(contract ISuperfluidToken token, bytes32 id, bytes32[] data) external
```

### updateAgreementDataFor

```solidity
function updateAgreementDataFor(contract ISuperfluidToken token, bytes32 id, bytes32[] data) external
```

### terminateAgreementFor

```solidity
function terminateAgreementFor(contract ISuperfluidToken token, bytes32 id, uint256 dataLength) external
```

### updateAgreementStateSlotFor

```solidity
function updateAgreementStateSlotFor(contract ISuperfluidToken token, address account, uint256 slotId, bytes32[] slotData) external
```

### settleBalanceFor

```solidity
function settleBalanceFor(contract ISuperfluidToken token, address account, int256 delta) external
```

### makeLiquidationPayoutsFor

```solidity
function makeLiquidationPayoutsFor(contract ISuperfluidToken token, bytes32 id, bool useDefaultRewardAccount, address liquidator, address targetAccount, uint256 rewardAmount, int256 targetAccountBalanceDelta) external
```

### tryCallAppBeforeCallback

```solidity
function tryCallAppBeforeCallback(contract ISuperfluid host, contract ISuperApp appMock, bool hackCtx, bytes ctx) external returns (bytes newCtx)
```

Agreement Framework Mockings

### tryCallAppAfterCallback

```solidity
function tryCallAppAfterCallback(contract ISuperfluid host, contract ISuperApp appMock, bool hackCtx, bytes ctx) external returns (bytes newCtx)
```

### tryAppCallbackPush

```solidity
function tryAppCallbackPush(contract ISuperfluid host, contract ISuperApp appMock, bool hackCtx, bytes ctx) external returns (bytes newCtx)
```

### tryAppCallbackPop

```solidity
function tryAppCallbackPop(contract ISuperfluid host, bytes ctx) external returns (bytes newCtx)
```

### tryCtxUseAllowance

```solidity
function tryCtxUseAllowance(contract ISuperfluid host, bool hackCtx, bytes ctx) external returns (bytes newCtx)
```

### tryJailApp

```solidity
function tryJailApp(contract ISuperfluid host, contract ISuperApp appMock, bool hackCtx, bytes ctx) external returns (bytes newCtx)
```

### requireValidCtx

```solidity
modifier requireValidCtx(bytes ctx)
```

Trivial Agreement Operations

### doRevert

```solidity
function doRevert(string reason, bytes ctx) external view
```

doRevert agreement operation

### Pong

```solidity
event Pong(uint256 ping)
```

pingMe agreement operation, emits Pong event

### pingMe

```solidity
function pingMe(address expectedMsgSender, uint256 ping, bytes ctx) external returns (bytes newCtx)
```

### AppBeforeCallbackResult

```solidity
event AppBeforeCallbackResult(uint8 appLevel, uint8 callType, bytes4 agreementSelector, bytes cbdata)
```

_callAppBeforeCallback base agreement operation, emits AppBeforeCallbackResult event

### _callAppBeforeCallback

```solidity
function _callAppBeforeCallback(contract ISuperApp app, uint256 noopBit, bytes ctx) private
```

### AppAfterCallbackResult

```solidity
event AppAfterCallbackResult(uint8 appLevel, uint8 callType, bytes4 agreementSelector)
```

_callAppAfterAgreementCallback base agreement operation, emits AppAfterCallbackResult event

### _callAppAfterAgreementCallback

```solidity
function _callAppAfterAgreementCallback(contract ISuperApp app, uint256 noopBit, bytes ctx) private returns (bytes newCtx)
```

### callAppBeforeAgreementCreatedCallback

```solidity
function callAppBeforeAgreementCreatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

### callAppAfterAgreementCreatedCallback

```solidity
function callAppAfterAgreementCreatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

### callAppBeforeAgreementUpdatedCallback

```solidity
function callAppBeforeAgreementUpdatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

### callAppAfterAgreementUpdatedCallback

```solidity
function callAppAfterAgreementUpdatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

### callAppBeforeAgreementTerminatedCallback

```solidity
function callAppBeforeAgreementTerminatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

### callAppAfterAgreementTerminatedCallback

```solidity
function callAppAfterAgreementTerminatedCallback(contract ISuperApp app, bytes ctx) external returns (bytes newCtx)
```

