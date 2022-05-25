# Solidity API

## ISuperApp

_Be aware of the app being jailed, when the word permitted is used._

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view returns (bytes cbdata)
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
function afterAgreementCreated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external returns (bytes newCtx)
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
function beforeAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view returns (bytes cbdata)
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
| cbdata | bytes | A free format in memory data the app can use to pass          arbitary information to the after-hook callback. NOTE: - It will be invoked with &#x60;staticcall&#x60;, no state changes are permitted. - Only revert with a &quot;reason&quot; is permitted. |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external returns (bytes newCtx)
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
function beforeAgreementTerminated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes ctx) external view returns (bytes cbdata)
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
| cbdata | bytes | A free format in memory data the app can use to pass          arbitary information to the after-hook callback. NOTE: - It will be invoked with &#x60;staticcall&#x60;, no state changes are permitted. - Revert is not permitted. |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external returns (bytes newCtx)
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

