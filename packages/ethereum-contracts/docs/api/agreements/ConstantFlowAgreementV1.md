# Solidity API

## ConstantFlowAgreementV1

_Please read IConstantFlowAgreementV1 for implementation notes.
For more technical notes, please visit protocol-monorepo wiki area._

### DEFAULT_MINIMUM_DEPOSIT

```solidity
uint256 DEFAULT_MINIMUM_DEPOSIT
```

_Default minimum deposit value

NOTE:
- It may come as a surprise that it is not 0, this is the minimum friction we have in the system for the
  imperfect blockchain system we live in.
- It is related to deposit clipping, and it is always rounded-up when clipping._

### MAXIMUM_DEPOSIT

```solidity
uint256 MAXIMUM_DEPOSIT
```

_Maximum deposit value_

### MAXIMUM_FLOW_RATE

```solidity
uint256 MAXIMUM_FLOW_RATE
```

_Maximum flow rate_

### CFAV1_PPP_CONFIG_KEY

```solidity
bytes32 CFAV1_PPP_CONFIG_KEY
```

### SUPERTOKEN_MINIMUM_DEPOSIT_KEY

```solidity
bytes32 SUPERTOKEN_MINIMUM_DEPOSIT_KEY
```

### FlowData

```solidity
struct FlowData {
  uint256 timestamp;
  int96 flowRate;
  uint256 deposit;
  uint256 owedDeposit;
}
```

### FlowParams

```solidity
struct FlowParams {
  bytes32 flowId;
  address sender;
  address receiver;
  address flowOperator;
  int96 flowRate;
  bytes userData;
}
```

### FlowOperatorData

```solidity
struct FlowOperatorData {
  uint8 permissions;
  int96 flowRateAllowance;
}
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(contract ISuperfluidToken token, address account, uint256 time) external view returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

_ISuperAgreement.realtimeBalanceOf implementation_

### _getMaximumFlowRateFromDepositPure

```solidity
function _getMaximumFlowRateFromDepositPure(uint256 liquidationPeriod, uint256 deposit) internal pure returns (int96 flowRate)
```

### _getDepositRequiredForFlowRatePure

```solidity
function _getDepositRequiredForFlowRatePure(uint256 minimumDeposit, uint256 liquidationPeriod, int96 flowRate) internal pure returns (uint256 deposit)
```

### getMaximumFlowRateFromDeposit

```solidity
function getMaximumFlowRateFromDeposit(contract ISuperfluidToken token, uint256 deposit) external view returns (int96 flowRate)
```

_IConstantFlowAgreementV1.getMaximumFlowRateFromDeposit implementation_

### getDepositRequiredForFlowRate

```solidity
function getDepositRequiredForFlowRate(contract ISuperfluidToken token, int96 flowRate) external view returns (uint256 deposit)
```

_IConstantFlowAgreementV1.getDepositRequiredForFlowRate implementation_

### isPatricianPeriodNow

```solidity
function isPatricianPeriodNow(contract ISuperfluidToken token, address account) public view returns (bool isCurrentlyPatricianPeriod, uint256 timestamp)
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
function isPatricianPeriod(contract ISuperfluidToken token, address account, uint256 timestamp) public view returns (bool)
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

### createFlow

```solidity
function createFlow(contract ISuperfluidToken token, address receiver, int96 flowRate, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.createFlow implementation_

### updateFlow

```solidity
function updateFlow(contract ISuperfluidToken token, address receiver, int96 flowRate, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.updateFlow implementation_

### deleteFlow

```solidity
function deleteFlow(contract ISuperfluidToken token, address sender, address receiver, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.deleteFlow implementation_

### getFlow

```solidity
function getFlow(contract ISuperfluidToken token, address sender, address receiver) external view returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

_IConstantFlowAgreementV1.getFlow implementation_

### getFlowByID

```solidity
function getFlowByID(contract ISuperfluidToken token, bytes32 flowId) external view returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

_IConstantFlowAgreementV1.getFlow implementation_

### getAccountFlowInfo

```solidity
function getAccountFlowInfo(contract ISuperfluidToken token, address account) external view returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

_IConstantFlowAgreementV1.getAccountFlowInfo implementation_

### getNetFlow

```solidity
function getNetFlow(contract ISuperfluidToken token, address account) external view returns (int96 flowRate)
```

_IConstantFlowAgreementV1.getNetFlow implementation_

### _StackVars_createOrUpdateFlow

```solidity
struct _StackVars_createOrUpdateFlow {
  contract ISuperfluidToken token;
  address sender;
  address receiver;
  int96 flowRate;
}
```

### _createOrUpdateFlowCheck

```solidity
function _createOrUpdateFlowCheck(struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars, struct ISuperfluid.Context currentContext) internal pure returns (bytes32 flowId, struct ConstantFlowAgreementV1.FlowParams flowParams)
```

_Checks conditions for both create/update flow
returns the flowId and flowParams_

### _createFlow

```solidity
function _createFlow(struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars, bytes ctx, struct ISuperfluid.Context currentContext) internal returns (bytes newCtx)
```

### _updateFlow

```solidity
function _updateFlow(struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars, struct ConstantFlowAgreementV1.FlowData oldFlowData, bool exist, bytes ctx, struct ISuperfluid.Context currentContext) internal returns (bytes newCtx)
```

### _deleteFlow

```solidity
function _deleteFlow(struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars, bool hasPermissions, bytes ctx, struct ISuperfluid.Context currentContext) internal returns (bytes newCtx)
```

### createFlowByOperator

```solidity
function createFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, int96 flowRate, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.createFlowByOperator implementation_

### updateFlowByOperator

```solidity
function updateFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, int96 flowRate, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.updateFlowByOperator implementation_

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(contract ISuperfluidToken token, address sender, address receiver, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.deleteFlowByOperator implementation_

### updateFlowOperatorPermissions

```solidity
function updateFlowOperatorPermissions(contract ISuperfluidToken token, address flowOperator, uint8 permissions, int96 flowRateAllowance, bytes ctx) public returns (bytes newCtx)
```

_IConstantFlowAgreementV1.updateFlowOperatorPermissions implementation_

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(contract ISuperfluidToken token, address flowOperator, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.authorizeFlowOperatorWithFullControl implementation_

### revokeFlowOperatorWithFullControl

```solidity
function revokeFlowOperatorWithFullControl(contract ISuperfluidToken token, address flowOperator, bytes ctx) external returns (bytes newCtx)
```

_IConstantFlowAgreementV1.revokeFlowOperatorWithFullControl implementation_

### getFlowOperatorData

```solidity
function getFlowOperatorData(contract ISuperfluidToken token, address sender, address flowOperator) public view returns (bytes32 flowOperatorId, uint8 permissions, int96 flowRateAllowance)
```

_IConstantFlowAgreementV1.getFlowOperatorData implementation_

### getFlowOperatorDataByID

```solidity
function getFlowOperatorDataByID(contract ISuperfluidToken token, bytes32 flowOperatorId) external view returns (uint8 permissions, int96 flowRateAllowance)
```

_IConstantFlowAgreementV1.getFlowOperatorDataByID implementation_

### FlowChangeType

```solidity
enum FlowChangeType {
  CREATE_FLOW,
  UPDATE_FLOW,
  DELETE_FLOW
}
```

### _getAccountFlowState

```solidity
function _getAccountFlowState(contract ISuperfluidToken token, address account) private view returns (bool exist, struct ConstantFlowAgreementV1.FlowData)
```

### _getAgreementData

```solidity
function _getAgreementData(contract ISuperfluidToken token, bytes32 dId) private view returns (bool exist, struct ConstantFlowAgreementV1.FlowData)
```

### _getFlowOperatorData

```solidity
function _getFlowOperatorData(contract ISuperfluidToken token, bytes32 flowOperatorId) private view returns (bool exist, struct ConstantFlowAgreementV1.FlowOperatorData)
```

### _updateFlowRateAllowance

```solidity
function _updateFlowRateAllowance(contract ISuperfluidToken token, bytes32 flowOperatorId, uint8 existingPermissions, int96 updatedFlowRateAllowance) private
```

### _updateAccountFlowState

```solidity
function _updateAccountFlowState(contract ISuperfluidToken token, address account, int96 flowRateDelta, int256 depositDelta, int256 owedDepositDelta, uint256 currentTimestamp) private returns (int96 newNetFlowRate)
```

### _changeFlowToNonApp

```solidity
function _changeFlowToNonApp(contract ISuperfluidToken token, struct ConstantFlowAgreementV1.FlowParams flowParams, struct ConstantFlowAgreementV1.FlowData oldFlowData, bytes ctx, struct ISuperfluid.Context currentContext) private returns (bytes newCtx)
```

_update a flow to a non-app receiver_

### _StackVars_changeFlowToApp

```solidity
struct _StackVars_changeFlowToApp {
  bytes cbdata;
  struct ConstantFlowAgreementV1.FlowData newFlowData;
  struct ISuperfluid.Context appContext;
}
```

### _changeFlowToApp

```solidity
function _changeFlowToApp(address appToCallback, contract ISuperfluidToken token, struct ConstantFlowAgreementV1.FlowParams flowParams, struct ConstantFlowAgreementV1.FlowData oldFlowData, bytes ctx, struct ISuperfluid.Context currentContext, enum ConstantFlowAgreementV1.FlowChangeType optype) private returns (bytes newCtx)
```

### _StackVars_changeFlow

```solidity
struct _StackVars_changeFlow {
  int96 totalSenderFlowRate;
  int96 totalReceiverFlowRate;
}
```

### _changeFlow

```solidity
function _changeFlow(uint256 currentTimestamp, contract ISuperfluidToken appAllowanceToken, contract ISuperfluidToken token, struct ConstantFlowAgreementV1.FlowParams flowParams, struct ConstantFlowAgreementV1.FlowData oldFlowData) private returns (int256 depositDelta, uint256 appAllowanceBase, struct ConstantFlowAgreementV1.FlowData newFlowData)
```

_change flow between sender and receiver with new flow rate

NOTE:
- leaving owed deposit unchanged for later adjustment
- depositDelta output is always clipped (see _clipDepositNumber)_

### _requireAvailableBalance

```solidity
function _requireAvailableBalance(contract ISuperfluidToken token, struct ISuperfluid.Context currentContext) private view
```

### _makeLiquidationPayouts

```solidity
function _makeLiquidationPayouts(contract ISuperfluidToken token, int256 availableBalance, struct ConstantFlowAgreementV1.FlowParams flowParams, struct ConstantFlowAgreementV1.FlowData flowData, address liquidator) private
```

### _clipDepositNumberRoundingDown

```solidity
function _clipDepositNumberRoundingDown(uint256 deposit) internal pure returns (uint256)
```

### _clipDepositNumber

```solidity
function _clipDepositNumber(uint256 deposit) internal pure returns (uint256)
```

### _calculateDeposit

```solidity
function _calculateDeposit(int96 flowRate, uint256 liquidationPeriod) internal pure returns (uint256 deposit)
```

### _generateFlowId

```solidity
function _generateFlowId(address sender, address receiver) private pure returns (bytes32 id)
```

### _encodeFlowData

```solidity
function _encodeFlowData(struct ConstantFlowAgreementV1.FlowData flowData) internal pure returns (bytes32[] data)
```

### _decodeFlowData

```solidity
function _decodeFlowData(uint256 wordA) internal pure returns (bool exist, struct ConstantFlowAgreementV1.FlowData flowData)
```

### _decode3PsData

```solidity
function _decode3PsData(contract ISuperfluidToken token) internal view returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

### _isPatricianPeriod

```solidity
function _isPatricianPeriod(int256 availableBalance, int256 signedTotalCFADeposit, uint256 liquidationPeriod, uint256 patricianPeriod) internal pure returns (bool)
```

### _generateFlowOperatorId

```solidity
function _generateFlowOperatorId(address sender, address flowOperator) private pure returns (bytes32 id)
```

### _encodeFlowOperatorData

```solidity
function _encodeFlowOperatorData(struct ConstantFlowAgreementV1.FlowOperatorData flowOperatorData) internal pure returns (bytes32[] data)
```

### _decodeFlowOperatorData

```solidity
function _decodeFlowOperatorData(uint256 wordA) internal pure returns (bool exist, struct ConstantFlowAgreementV1.FlowOperatorData flowOperatorData)
```

### _getBooleanFlowOperatorPermissions

```solidity
function _getBooleanFlowOperatorPermissions(uint8 permissions, enum ConstantFlowAgreementV1.FlowChangeType flowChangeType) internal pure returns (bool flowchangeTypeAllowed)
```

