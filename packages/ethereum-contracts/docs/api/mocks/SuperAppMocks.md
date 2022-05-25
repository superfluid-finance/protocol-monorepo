# Solidity API

## SuperAppMockAux

### actionPingAgreement

```solidity
function actionPingAgreement(contract ISuperfluid host, contract AgreementMock agreement, uint256 ping, bytes ctx) external
```

### actionCallActionNoop

```solidity
function actionCallActionNoop(contract ISuperfluid host, contract SuperAppMock app, bytes ctx) external
```

## SuperAppMock

### _host

```solidity
contract ISuperfluid _host
```

### _aux

```solidity
contract SuperAppMockAux _aux
```

### constructor

```solidity
constructor(contract ISuperfluid host, uint256 configWord, bool doubleRegistration) public
```

### tryRegisterApp

```solidity
function tryRegisterApp(uint256 configWord) external
```

### allowCompositeApp

```solidity
function allowCompositeApp(contract ISuperApp target) external
```

### NoopEvent

```solidity
event NoopEvent(uint8 appLevel, uint8 callType, bytes4 agreementSelector)
```

### actionNoop

```solidity
function actionNoop(bytes ctx) external returns (bytes newCtx)
```

### actionExpectMsgSender

```solidity
function actionExpectMsgSender(address expectedMsgSender, bytes ctx) external returns (bytes newCtx)
```

### actionAssert

```solidity
function actionAssert(bytes ctx) external view
```

### actionRevert

```solidity
function actionRevert(bytes ctx) external view
```

### actionRevertWithReason

```solidity
function actionRevertWithReason(string reason, bytes ctx) external view
```

### actionCallAgreementWithoutCtx

```solidity
function actionCallAgreementWithoutCtx(bytes ctx) external
```

### actionCallAppActionWithoutCtx

```solidity
function actionCallAppActionWithoutCtx(bytes ctx) external
```

### actionAlteringCtx

```solidity
function actionAlteringCtx(bytes ctx) external view returns (bytes newCtx)
```

### actionReturnEmptyCtx

```solidity
function actionReturnEmptyCtx(bytes ctx) external view
```

### actionPingAgreementThroughAux

```solidity
function actionPingAgreementThroughAux(contract AgreementMock agreement, uint256 ping, bytes ctx) external
```

### actionCallActionNoopThroughAux

```solidity
function actionCallActionNoopThroughAux(bytes ctx) external
```

### actionPingAgreement

```solidity
function actionPingAgreement(contract AgreementMock agreement, uint256 ping, bytes ctx) external returns (bytes newCtx)
```

### actionAgreementRevert

```solidity
function actionAgreementRevert(contract AgreementMock agreement, string reason, bytes ctx) external returns (bytes newCtx)
```

### actionCallActionNoop

```solidity
function actionCallActionNoop(bytes ctx) external returns (bytes newCtx)
```

### actionCallActionRevert

```solidity
function actionCallActionRevert(string reason, bytes ctx) external returns (bytes newCtx)
```

### actionCallAgreementWithInvalidCtx

```solidity
function actionCallAgreementWithInvalidCtx(contract AgreementMock agreement, bytes ctx) external returns (bytes newCtx)
```

### actionCallActionWithInvalidCtx

```solidity
function actionCallActionWithInvalidCtx(string reason, bytes ctx) external returns (bytes newCtx)
```

### actionCallBadAction

```solidity
function actionCallBadAction(bytes ctx) external
```

### NextCallbackActionType

```solidity
enum NextCallbackActionType {
  Noop,
  Assert,
  Revert,
  RevertWithReason,
  AlteringCtx,
  BurnGas,
  ReturnEmptyCtx
}
```

### NextCallbackAction

```solidity
struct NextCallbackAction {
  enum SuperAppMock.NextCallbackActionType actionType;
  bytes data;
}
```

### _nextCallbackAction

```solidity
struct SuperAppMock.NextCallbackAction _nextCallbackAction
```

### setNextCallbackAction

```solidity
function setNextCallbackAction(enum SuperAppMock.NextCallbackActionType actionType, bytes data) external
```

### _executeBeforeCallbackAction

```solidity
function _executeBeforeCallbackAction() private view returns (bytes cbdata)
```

### _executeAfterCallbackAction

```solidity
function _executeAfterCallbackAction(bytes ctx) private returns (bytes newCtx)
```

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes ctx) external view virtual returns (bytes)
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes, bytes ctx) external virtual returns (bytes newCtx)
```

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(contract ISuperToken, address, bytes32, bytes, bytes ctx) external view virtual returns (bytes)
```

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken, address, bytes32, bytes, bytes, bytes ctx) external virtual returns (bytes newCtx)
```

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(contract ISuperToken, address, bytes32, bytes, bytes ctx) external view virtual returns (bytes)
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken, address, bytes32, bytes, bytes, bytes ctx) external virtual returns (bytes newCtx)
```

### _burnGas

```solidity
function _burnGas(uint256 gasToBurn) private view
```

### requireValidCtx

```solidity
modifier requireValidCtx(bytes ctx)
```

## SuperAppMockReturningEmptyCtx

### _host

```solidity
contract ISuperfluid _host
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes) external pure
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes, bytes) external pure
```

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(contract ISuperToken, address, bytes32, bytes, bytes) external pure
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken, address, bytes32, bytes, bytes, bytes) external pure
```

## SuperAppMockReturningInvalidCtx

### _host

```solidity
contract ISuperfluid _host
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes, bytes) external pure returns (uint256)
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken, address, bytes32, bytes, bytes, bytes) external pure returns (uint256)
```

## SuperAppMock2ndLevel

### _host

```solidity
contract ISuperfluid _host
```

### _app

```solidity
contract SuperAppMock _app
```

### _agreement

```solidity
contract AgreementMock _agreement
```

### constructor

```solidity
constructor(contract ISuperfluid host, contract SuperAppMock app, contract AgreementMock agreement) public
```

### allowCompositeApp

```solidity
function allowCompositeApp() external
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

## SuperAppMockWithRegistrationkey

### _host

```solidity
contract ISuperfluid _host
```

### constructor

```solidity
constructor(contract ISuperfluid host, uint256 configWord, string registrationKey) public
```

## SuperAppMockNotSelfRegistering

## SuperAppFactoryMock

### registerAppWithHost

```solidity
function registerAppWithHost(contract ISuperfluid host, contract ISuperApp app, uint256 configWord) external
```

