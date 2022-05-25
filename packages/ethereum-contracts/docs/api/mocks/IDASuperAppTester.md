# Solidity API

## IDASuperAppTester

### _host

```solidity
contract ISuperfluid _host
```

### _ida

```solidity
contract IInstantDistributionAgreementV1 _ida
```

### _token

```solidity
contract ISuperToken _token
```

### _indexId

```solidity
uint32 _indexId
```

### constructor

```solidity
constructor(contract ISuperfluid host, uint256 configWord, contract IInstantDistributionAgreementV1 ida, contract ISuperToken token, uint32 indexId) public
```

### updateSubscription

```solidity
function updateSubscription(address subscriber, uint128 units) external
```

### distribute

```solidity
function distribute(uint128 amount) external
```

### _expectCallback

```solidity
function _expectCallback(bytes ctx, bytes32 callbackType, bytes agreementData) private view
```

### SubscriptionDataBefore

```solidity
event SubscriptionDataBefore(address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

### SubscriptionDataAfter

```solidity
event SubscriptionDataAfter(address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

### _packSubscriptionData

```solidity
function _packSubscriptionData(bytes32 agreementId) private view returns (bytes)
```

### _forceGetSubscriptionByID

```solidity
bool _forceGetSubscriptionByID
```

### setForceGetSubscriptionByID

```solidity
function setForceGetSubscriptionByID() external
```

### _emitSubscriptionDataEvents

```solidity
function _emitSubscriptionDataEvents(bytes cbdata, bytes32 agreementId, bool deleted) private
```

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view virtual returns (bytes cbdata)
```

_Callback before a new agreement is created._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| cbdata | bytes | A free format in memory data the app can use to pass          arbitary information to the after-hook callback. NOTE: - It will be invoked with &#x60;staticcall&#x60;, no state changes are permitted. - Only revert with a &quot;reason&quot; is permitted. |

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external virtual returns (bytes newCtx)
```

_Callback after a new agreement is created._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| cbdata | bytes | The data returned from the before-hook callback. |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. NOTE: - State changes is permitted. - Only revert with a &quot;reason&quot; is permitted. |

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view virtual returns (bytes)
```

_Callback before a new agreement is updated._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bytes |  |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external virtual returns (bytes newCtx)
```

_Callback after a new agreement is updated._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| cbdata | bytes | The data returned from the before-hook callback. |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. NOTE: - State changes is permitted. - Only revert with a &quot;reason&quot; is permitted. |

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view virtual returns (bytes)
```

_Callback before a new agreement is terminated._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external virtual returns (bytes newCtx)
```

_Callback after a new agreement is terminated._

| Name | Type | Description |
| ---- | ---- | ----------- |
| superToken | contract ISuperToken | The super token used for the agreement. |
| agreementClass | address | The agreement class address. |
| agreementId | bytes32 | The agreementId |
| agreementData | bytes | The agreement data (non-compressed) |
| cbdata | bytes | The data returned from the before-hook callback. |
| ctx | bytes | The context data. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. NOTE: - State changes is permitted. - Revert is not permitted. |

### requireValidCtx

```solidity
modifier requireValidCtx(bytes ctx)
```

### onlyExpected

```solidity
modifier onlyExpected(contract ISuperToken superToken, address agreementClass)
```

