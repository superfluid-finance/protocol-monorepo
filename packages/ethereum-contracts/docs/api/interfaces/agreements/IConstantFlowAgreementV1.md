# Solidity API

## IConstantFlowAgreementV1

### agreementType

```solidity
function agreementType() external pure returns (bytes32)
```

_ISuperAgreement.agreementType implementation_

### getMaximumFlowRateFromDeposit

```solidity
function getMaximumFlowRateFromDeposit(contract ISuperfluidToken token, uint256 deposit) external view virtual returns (int96 flowRate)
```

Get the maximum flow rate allowed with the deposit

_The deposit is clipped and rounded down_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken |  |
| deposit | uint256 | Deposit amount used for creating the flow |

| Name | Type | Description |
| ---- | ---- | ----------- |
| flowRate | int96 | The maximum flow rate |

### getDepositRequiredForFlowRate

```solidity
function getDepositRequiredForFlowRate(contract ISuperfluidToken token, int96 flowRate) external view virtual returns (uint256 deposit)
```

Get the deposit required for creating the flow

_Calculates the deposit based on the liquidationPeriod and flowRate_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken |  |
| flowRate | int96 | Flow rate to be tested |

| Name | Type | Description |
| ---- | ---- | ----------- |
| deposit | uint256 | The deposit amount based on flowRate and liquidationPeriod NOTE: - if calculated deposit (flowRate * liquidationPeriod) is less   than the minimum deposit, we use the minimum deposit otherwise   we use the calculated deposit |

### isPatricianPeriodNow

```solidity
function isPatricianPeriodNow(contract ISuperfluidToken token, address account) public view virtual returns (bool isCurrentlyPatricianPeriod, uint256 timestamp)
```

_Returns whether it is the patrician period based on host.getNow()_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken |  |
| account | address | The account we are interested in |

| Name | Type | Description |
| ---- | ---- | ----------- |
| isCurrentlyPatricianPeriod | bool | Whether it is currently the patrician period dictated by governance |
| timestamp | uint256 | The value of host.getNow() |

### isPatricianPeriod

```solidity
function isPatricianPeriod(contract ISuperfluidToken token, address account, uint256 timestamp) public view virtual returns (bool)
```

_Returns whether it is the patrician period based on timestamp_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken |  |
| account | address | The account we are interested in |
| timestamp | uint256 | The timestamp we are interested in observing the result of isPatricianPeriod |

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bool | bool Whether it is currently the patrician period dictated by governance |

### updateFlowOperatorPermissions

```solidity
function updateFlowOperatorPermissions(contract ISuperfluidToken token, address flowOperator, uint8 permissions, int96 flowRateAllowance, bytes ctx) external virtual returns (bytes newCtx)
```

_msgSender from &#x60;ctx&#x60; updates permissions for the &#x60;flowOperator&#x60; with &#x60;flowRateAllowance&#x60;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| flowOperator | address | The permission grantee address |
| permissions | uint8 | A bitmask representation of the granted permissions |
| flowRateAllowance | int96 | The flow rate allowance the &#x60;flowOperator&#x60; is granted (only goes down) |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(contract ISuperfluidToken token, address flowOperator, bytes ctx) external virtual returns (bytes newCtx)
```

_msgSender from &#x60;ctx&#x60; grants &#x60;flowOperator&#x60; all permissions with flowRateAllowance as type(int96).max_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| flowOperator | address | The permission grantee address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### revokeFlowOperatorWithFullControl

```solidity
function revokeFlowOperatorWithFullControl(contract ISuperfluidToken token, address flowOperator, bytes ctx) external virtual returns (bytes newCtx)
```

msgSender from &#x60;ctx&#x60; revokes &#x60;flowOperator&#x60; create/update/delete permissions

_&#x60;permissions&#x60; and &#x60;flowRateAllowance&#x60; will both be set to 0_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| flowOperator | address | The permission grantee address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### getFlowOperatorData

```solidity
function getFlowOperatorData(contract ISuperfluidToken token, address sender, address flowOperator) public view virtual returns (bytes32 flowOperatorId, uint8 permissions, int96 flowRateAllowance)
```

Get the permissions of a flow operator between &#x60;sender&#x60; and &#x60;flowOperator&#x60; for &#x60;token&#x60;

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | The permission granter address |
| flowOperator | address | The permission grantee address |

| Name | Type | Description |
| ---- | ---- | ----------- |
| flowOperatorId | bytes32 | The keccak256 hash of encoded string &quot;flowOperator&quot;, sender and flowOperator |
| permissions | uint8 | A bitmask representation of the granted permissions |
| flowRateAllowance | int96 | The flow rate allowance the &#x60;flowOperator&#x60; is granted (only goes down) |

### getFlowOperatorDataByID

```solidity
function getFlowOperatorDataByID(contract ISuperfluidToken token, bytes32 flowOperatorId) external view virtual returns (uint8 permissions, int96 flowRateAllowance)
```

Get flow operator using flowOperatorId

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| flowOperatorId | bytes32 | The keccak256 hash of encoded string &quot;flowOperator&quot;, sender and flowOperator |

| Name | Type | Description |
| ---- | ---- | ----------- |
| permissions | uint8 | A bitmask representation of the granted permissions |
| flowRateAllowance | int96 | The flow rate allowance the &#x60;flowOperator&#x60; is granted (only goes down) |

### createFlow

```solidity
function createFlow(contract ISuperfluidToken token, address receiver, int96 flowRate, bytes ctx) external virtual returns (bytes newCtx)
```

Create a flow betwen ctx.msgSender and receiver

_flowId (agreementId) is the keccak256 hash of encoded sender and receiver_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| receiver | address | Flow receiver address |
| flowRate | int96 | New flow rate in amount per second |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - AgreementCreated   - agreementId - can be used in getFlowByID   - agreementData - abi.encode(address flowSender, address flowReceiver) NOTE: - A deposit is taken as safety margin for the solvency agents - A extra gas fee may be taken to pay for solvency agent liquidations |

### createFlowByOperator

```solidity
function createFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, int96 flowRate, bytes ctx) external virtual returns (bytes newCtx)
```

Create a flow between sender and receiver

_A flow created by an approved flow operator (see above for details on callbacks)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | Flow sender address (has granted permissions) |
| receiver | address | Flow receiver address |
| flowRate | int96 | New flow rate in amount per second |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### updateFlow

```solidity
function updateFlow(contract ISuperfluidToken token, address receiver, int96 flowRate, bytes ctx) external virtual returns (bytes newCtx)
```

Update the flow rate between ctx.msgSender and receiver

_flowId (agreementId) is the keccak256 hash of encoded sender and receiver_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| receiver | address | Flow receiver address |
| flowRate | int96 | New flow rate in amount per second |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - AgreementUpdated   - agreementId - can be used in getFlowByID   - agreementData - abi.encode(address flowSender, address flowReceiver) NOTE: - Only the flow sender may update the flow rate - Even if the flow rate is zero, the flow is not deleted from the system - Deposit amount will be adjusted accordingly - No new gas fee is charged |

### updateFlowByOperator

```solidity
function updateFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, int96 flowRate, bytes ctx) external virtual returns (bytes newCtx)
```

Update a flow between sender and receiver

_A flow updated by an approved flow operator (see above for details on callbacks)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | Flow sender address (has granted permissions) |
| receiver | address | Flow receiver address |
| flowRate | int96 | New flow rate in amount per second |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### getFlow

```solidity
function getFlow(contract ISuperfluidToken token, address sender, address receiver) external view virtual returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

_Get the flow data between &#x60;sender&#x60; and &#x60;receiver&#x60; of &#x60;token&#x60;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | Flow receiver |
| receiver | address | Flow sender |

| Name | Type | Description |
| ---- | ---- | ----------- |
| timestamp | uint256 | Timestamp of when the flow is updated |
| flowRate | int96 | The flow rate |
| deposit | uint256 | The amount of deposit the flow |
| owedDeposit | uint256 | The amount of owed deposit of the flow |

### getFlowByID

```solidity
function getFlowByID(contract ISuperfluidToken token, bytes32 agreementId) external view virtual returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

Get flow data using agreementId

_flowId (agreementId) is the keccak256 hash of encoded sender and receiver_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| agreementId | bytes32 | The agreement ID |

| Name | Type | Description |
| ---- | ---- | ----------- |
| timestamp | uint256 | Timestamp of when the flow is updated |
| flowRate | int96 | The flow rate |
| deposit | uint256 | The deposit amount of the flow |
| owedDeposit | uint256 | The owed deposit amount of the flow |

### getAccountFlowInfo

```solidity
function getAccountFlowInfo(contract ISuperfluidToken token, address account) external view virtual returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

_Get the aggregated flow info of the account_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| account | address | Account for the query |

| Name | Type | Description |
| ---- | ---- | ----------- |
| timestamp | uint256 | Timestamp of when a flow was last updated for account |
| flowRate | int96 | The net flow rate of token for account |
| deposit | uint256 | The sum of all deposits for account&#x27;s flows |
| owedDeposit | uint256 | The sum of all owed deposits for account&#x27;s flows |

### getNetFlow

```solidity
function getNetFlow(contract ISuperfluidToken token, address account) external view virtual returns (int96 flowRate)
```

_Get the net flow rate of the account_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| account | address | Account for the query |

| Name | Type | Description |
| ---- | ---- | ----------- |
| flowRate | int96 | Net flow rate |

### deleteFlow

```solidity
function deleteFlow(contract ISuperfluidToken token, address sender, address receiver, bytes ctx) external virtual returns (bytes newCtx)
```

Delete the flow between sender and receiver

_flowId (agreementId) is the keccak256 hash of encoded sender and receiver_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address |  |
| receiver | address | Flow receiver address # App callbacks - AgreementTerminated   - agreementId - can be used in getFlowByID   - agreementData - abi.encode(address flowSender, address flowReceiver) NOTE: - Both flow sender and receiver may delete the flow - If Sender account is insolvent or in critical state, a solvency agent may   also terminate the agreement - Gas fee may be returned to the sender |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, bytes ctx) external virtual returns (bytes newCtx)
```

Delete the flow between sender and receiver

_A flow deleted by an approved flow operator (see above for details on callbacks)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address |  |
| receiver | address | Flow receiver address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) |

### FlowOperatorUpdated

```solidity
event FlowOperatorUpdated(contract ISuperfluidToken token, address sender, address flowOperator, uint8 permissions, int96 flowRateAllowance)
```

_Flow operator updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | Flow sender address |
| flowOperator | address | Flow operator address |
| permissions | uint8 | Octo bitmask representation of permissions |
| flowRateAllowance | int96 | The flow rate allowance the &#x60;flowOperator&#x60; is granted (only goes down) |

### FlowUpdated

```solidity
event FlowUpdated(contract ISuperfluidToken token, address sender, address receiver, int96 flowRate, int256 totalSenderFlowRate, int256 totalReceiverFlowRate, bytes userData)
```

_Flow updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| sender | address | Flow sender address |
| receiver | address | Flow recipient address |
| flowRate | int96 | Flow rate in amount per second for this flow |
| totalSenderFlowRate | int256 | Total flow rate in amount per second for the sender |
| totalReceiverFlowRate | int256 | Total flow rate in amount per second for the receiver |
| userData | bytes | The user provided data |

### FlowUpdatedExtension

```solidity
event FlowUpdatedExtension(address flowOperator, uint256 deposit)
```

_Flow updated extension event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| flowOperator | address | Flow operator address - the Context.msgSender |
| deposit | uint256 | The deposit amount for the stream |

