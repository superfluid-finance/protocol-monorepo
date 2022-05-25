# ConstantFlowAgreementV1

Please read IConstantFlowAgreementV1 for implementation notes.
For more technical notes, please visit protocol-monorepo wiki area.

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

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(
    contract ISuperfluidToken token,
    address account,
    uint256 time
) external returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

ISuperAgreement.realtimeBalanceOf implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `time` | uint256 |  |

### _getMaximumFlowRateFromDepositPure

```solidity
function _getMaximumFlowRateFromDepositPure(
    uint256 liquidationPeriod,
    uint256 deposit
) internal returns (int96 flowRate)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `liquidationPeriod` | uint256 |  |
| `deposit` | uint256 |  |

### _getDepositRequiredForFlowRatePure

```solidity
function _getDepositRequiredForFlowRatePure(
    uint256 minimumDeposit,
    uint256 liquidationPeriod,
    int96 flowRate
) internal returns (uint256 deposit)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `minimumDeposit` | uint256 |  |
| `liquidationPeriod` | uint256 |  |
| `flowRate` | int96 |  |

### getMaximumFlowRateFromDeposit

```solidity
function getMaximumFlowRateFromDeposit(
    contract ISuperfluidToken token,
    uint256 deposit
) external returns (int96 flowRate)
```

IConstantFlowAgreementV1.getMaximumFlowRateFromDeposit implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `deposit` | uint256 |  |

### getDepositRequiredForFlowRate

```solidity
function getDepositRequiredForFlowRate(
    contract ISuperfluidToken token,
    int96 flowRate
) external returns (uint256 deposit)
```

IConstantFlowAgreementV1.getDepositRequiredForFlowRate implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowRate` | int96 |  |

### isPatricianPeriodNow

```solidity
function isPatricianPeriodNow(
    contract ISuperfluidToken token,
    address account
) public returns (bool isCurrentlyPatricianPeriod, uint256 timestamp)
```

Returns whether it is the patrician period based on host.getNow()

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address | The account we are interested in |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `isCurrentlyPatricianPeriod` | bool | Whether it is currently the patrician period dictated by governance |
| `timestamp` | uint256 | The value of host.getNow() |

### isPatricianPeriod

```solidity
function isPatricianPeriod(
    contract ISuperfluidToken token,
    address account,
    uint256 timestamp
) public returns (bool)
```

Returns whether it is the patrician period based on timestamp

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address | The account we are interested in |
| `timestamp` | uint256 | The timestamp we are interested in observing the result of isPatricianPeriod |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bool | bool Whether it is currently the patrician period dictated by governance |

### createFlow

```solidity
function createFlow(
    contract ISuperfluidToken token,
    address receiver,
    int96 flowRate,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.createFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |
| `ctx` | bytes |  |

### updateFlow

```solidity
function updateFlow(
    contract ISuperfluidToken token,
    address receiver,
    int96 flowRate,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.updateFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |
| `ctx` | bytes |  |

### deleteFlow

```solidity
function deleteFlow(
    contract ISuperfluidToken token,
    address sender,
    address receiver,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.deleteFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `receiver` | address |  |
| `ctx` | bytes |  |

### getFlow

```solidity
function getFlow(
    contract ISuperfluidToken token,
    address sender,
    address receiver
) external returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

IConstantFlowAgreementV1.getFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `receiver` | address |  |

### getFlowByID

```solidity
function getFlowByID(
    contract ISuperfluidToken token,
    bytes32 flowId
) external returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

IConstantFlowAgreementV1.getFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowId` | bytes32 |  |

### getAccountFlowInfo

```solidity
function getAccountFlowInfo(
    contract ISuperfluidToken token,
    address account
) external returns (uint256 timestamp, int96 flowRate, uint256 deposit, uint256 owedDeposit)
```

IConstantFlowAgreementV1.getAccountFlowInfo implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |

### getNetFlow

```solidity
function getNetFlow(
    contract ISuperfluidToken token,
    address account
) external returns (int96 flowRate)
```

IConstantFlowAgreementV1.getNetFlow implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |

### _createOrUpdateFlowCheck

```solidity
function _createOrUpdateFlowCheck(
    struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars,
    struct ISuperfluid.Context currentContext
) internal returns (bytes32 flowId, struct ConstantFlowAgreementV1.FlowParams flowParams)
```

Checks conditions for both create/update flow
returns the flowId and flowParams

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowVars` | struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow |  |
| `currentContext` | struct ISuperfluid.Context |  |

### _createFlow

```solidity
function _createFlow(
    struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars,
    bytes ctx,
    struct ISuperfluid.Context currentContext
) internal returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowVars` | struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow |  |
| `ctx` | bytes |  |
| `currentContext` | struct ISuperfluid.Context |  |

### _updateFlow

```solidity
function _updateFlow(
    struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars,
    struct ConstantFlowAgreementV1.FlowData oldFlowData,
    bool exist,
    bytes ctx,
    struct ISuperfluid.Context currentContext
) internal returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowVars` | struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow |  |
| `oldFlowData` | struct ConstantFlowAgreementV1.FlowData |  |
| `exist` | bool |  |
| `ctx` | bytes |  |
| `currentContext` | struct ISuperfluid.Context |  |

### _deleteFlow

```solidity
function _deleteFlow(
    struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow flowVars,
    bool hasPermissions,
    bytes ctx,
    struct ISuperfluid.Context currentContext
) internal returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowVars` | struct ConstantFlowAgreementV1._StackVars_createOrUpdateFlow |  |
| `hasPermissions` | bool |  |
| `ctx` | bytes |  |
| `currentContext` | struct ISuperfluid.Context |  |

### createFlowByOperator

```solidity
function createFlowByOperator(
    contract ISuperfluidToken token,
    address sender,
    address receiver,
    int96 flowRate,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.createFlowByOperator implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |
| `ctx` | bytes |  |

### updateFlowByOperator

```solidity
function updateFlowByOperator(
    contract ISuperfluidToken token,
    address sender,
    address receiver,
    int96 flowRate,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.updateFlowByOperator implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |
| `ctx` | bytes |  |

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(
    contract ISuperfluidToken token,
    address sender,
    address receiver,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.deleteFlowByOperator implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `receiver` | address |  |
| `ctx` | bytes |  |

### updateFlowOperatorPermissions

```solidity
function updateFlowOperatorPermissions(
    contract ISuperfluidToken token,
    address flowOperator,
    uint8 permissions,
    int96 flowRateAllowance,
    bytes ctx
) public returns (bytes newCtx)
```

IConstantFlowAgreementV1.updateFlowOperatorPermissions implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperator` | address |  |
| `permissions` | uint8 |  |
| `flowRateAllowance` | int96 |  |
| `ctx` | bytes |  |

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(
    contract ISuperfluidToken token,
    address flowOperator,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.authorizeFlowOperatorWithFullControl implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperator` | address |  |
| `ctx` | bytes |  |

### revokeFlowOperatorWithFullControl

```solidity
function revokeFlowOperatorWithFullControl(
    contract ISuperfluidToken token,
    address flowOperator,
    bytes ctx
) external returns (bytes newCtx)
```

IConstantFlowAgreementV1.revokeFlowOperatorWithFullControl implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperator` | address |  |
| `ctx` | bytes |  |

### getFlowOperatorData

```solidity
function getFlowOperatorData(
    contract ISuperfluidToken token,
    address sender,
    address flowOperator
) public returns (bytes32 flowOperatorId, uint8 permissions, int96 flowRateAllowance)
```

IConstantFlowAgreementV1.getFlowOperatorData implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sender` | address |  |
| `flowOperator` | address |  |

### getFlowOperatorDataByID

```solidity
function getFlowOperatorDataByID(
    contract ISuperfluidToken token,
    bytes32 flowOperatorId
) external returns (uint8 permissions, int96 flowRateAllowance)
```

IConstantFlowAgreementV1.getFlowOperatorDataByID implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperatorId` | bytes32 |  |

### _getAccountFlowState

```solidity
function _getAccountFlowState(
    contract ISuperfluidToken token,
    address account
) private returns (bool exist, struct ConstantFlowAgreementV1.FlowData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |

### _getAgreementData

```solidity
function _getAgreementData(
    contract ISuperfluidToken token,
    bytes32 dId
) private returns (bool exist, struct ConstantFlowAgreementV1.FlowData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `dId` | bytes32 |  |

### _getFlowOperatorData

```solidity
function _getFlowOperatorData(
    contract ISuperfluidToken token,
    bytes32 flowOperatorId
) private returns (bool exist, struct ConstantFlowAgreementV1.FlowOperatorData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperatorId` | bytes32 |  |

### _updateFlowRateAllowance

```solidity
function _updateFlowRateAllowance(
    contract ISuperfluidToken token,
    bytes32 flowOperatorId,
    uint8 existingPermissions,
    int96 updatedFlowRateAllowance
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowOperatorId` | bytes32 |  |
| `existingPermissions` | uint8 |  |
| `updatedFlowRateAllowance` | int96 |  |

### _updateAccountFlowState

```solidity
function _updateAccountFlowState(
    contract ISuperfluidToken token,
    address account,
    int96 flowRateDelta,
    int256 depositDelta,
    int256 owedDepositDelta,
    uint256 currentTimestamp
) private returns (int96 newNetFlowRate)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `flowRateDelta` | int96 |  |
| `depositDelta` | int256 |  |
| `owedDepositDelta` | int256 |  |
| `currentTimestamp` | uint256 |  |

### _changeFlowToNonApp

```solidity
function _changeFlowToNonApp(
    contract ISuperfluidToken token,
    struct ConstantFlowAgreementV1.FlowParams flowParams,
    struct ConstantFlowAgreementV1.FlowData oldFlowData,
    bytes ctx,
    struct ISuperfluid.Context currentContext
) private returns (bytes newCtx)
```

update a flow to a non-app receiver

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `flowParams` | struct ConstantFlowAgreementV1.FlowParams |  |
| `oldFlowData` | struct ConstantFlowAgreementV1.FlowData |  |
| `ctx` | bytes |  |
| `currentContext` | struct ISuperfluid.Context |  |

### _changeFlowToApp

```solidity
function _changeFlowToApp(
    address appToCallback,
    contract ISuperfluidToken token,
    struct ConstantFlowAgreementV1.FlowParams flowParams,
    struct ConstantFlowAgreementV1.FlowData oldFlowData,
    bytes ctx,
    struct ISuperfluid.Context currentContext,
    enum ConstantFlowAgreementV1.FlowChangeType optype
) private returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appToCallback` | address |  |
| `token` | contract ISuperfluidToken |  |
| `flowParams` | struct ConstantFlowAgreementV1.FlowParams |  |
| `oldFlowData` | struct ConstantFlowAgreementV1.FlowData |  |
| `ctx` | bytes |  |
| `currentContext` | struct ISuperfluid.Context |  |
| `optype` | enum ConstantFlowAgreementV1.FlowChangeType |  |

### _changeFlow

```solidity
function _changeFlow(
    uint256 currentTimestamp,
    contract ISuperfluidToken appAllowanceToken,
    contract ISuperfluidToken token,
    struct ConstantFlowAgreementV1.FlowParams flowParams,
    struct ConstantFlowAgreementV1.FlowData oldFlowData
) private returns (int256 depositDelta, uint256 appAllowanceBase, struct ConstantFlowAgreementV1.FlowData newFlowData)
```

change flow between sender and receiver with new flow rate

NOTE:
- leaving owed deposit unchanged for later adjustment
- depositDelta output is always clipped (see _clipDepositNumber)

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `currentTimestamp` | uint256 |  |
| `appAllowanceToken` | contract ISuperfluidToken |  |
| `token` | contract ISuperfluidToken |  |
| `flowParams` | struct ConstantFlowAgreementV1.FlowParams |  |
| `oldFlowData` | struct ConstantFlowAgreementV1.FlowData |  |

### _requireAvailableBalance

```solidity
function _requireAvailableBalance(
    contract ISuperfluidToken token,
    struct ISuperfluid.Context currentContext
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `currentContext` | struct ISuperfluid.Context |  |

### _makeLiquidationPayouts

```solidity
function _makeLiquidationPayouts(
    contract ISuperfluidToken token,
    int256 availableBalance,
    struct ConstantFlowAgreementV1.FlowParams flowParams,
    struct ConstantFlowAgreementV1.FlowData flowData,
    address liquidator
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `availableBalance` | int256 |  |
| `flowParams` | struct ConstantFlowAgreementV1.FlowParams |  |
| `flowData` | struct ConstantFlowAgreementV1.FlowData |  |
| `liquidator` | address |  |

### _clipDepositNumberRoundingDown

```solidity
function _clipDepositNumberRoundingDown(
    uint256 deposit
) internal returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `deposit` | uint256 |  |

### _clipDepositNumber

```solidity
function _clipDepositNumber(
    uint256 deposit
) internal returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `deposit` | uint256 |  |

### _calculateDeposit

```solidity
function _calculateDeposit(
    int96 flowRate,
    uint256 liquidationPeriod
) internal returns (uint256 deposit)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowRate` | int96 |  |
| `liquidationPeriod` | uint256 |  |

### _generateFlowId

```solidity
function _generateFlowId(
    address sender,
    address receiver
) private returns (bytes32 id)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `receiver` | address |  |

### _encodeFlowData

```solidity
function _encodeFlowData(
    struct ConstantFlowAgreementV1.FlowData flowData
) internal returns (bytes32[] data)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowData` | struct ConstantFlowAgreementV1.FlowData |  |

### _decodeFlowData

```solidity
function _decodeFlowData(
    uint256 wordA
) internal returns (bool exist, struct ConstantFlowAgreementV1.FlowData flowData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `wordA` | uint256 |  |

### _decode3PsData

```solidity
function _decode3PsData(
    contract ISuperfluidToken token
) internal returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |

### _isPatricianPeriod

```solidity
function _isPatricianPeriod(
    int256 availableBalance,
    int256 signedTotalCFADeposit,
    uint256 liquidationPeriod,
    uint256 patricianPeriod
) internal returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `availableBalance` | int256 |  |
| `signedTotalCFADeposit` | int256 |  |
| `liquidationPeriod` | uint256 |  |
| `patricianPeriod` | uint256 |  |

### _generateFlowOperatorId

```solidity
function _generateFlowOperatorId(
    address sender,
    address flowOperator
) private returns (bytes32 id)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `flowOperator` | address |  |

### _encodeFlowOperatorData

```solidity
function _encodeFlowOperatorData(
    struct ConstantFlowAgreementV1.FlowOperatorData flowOperatorData
) internal returns (bytes32[] data)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowOperatorData` | struct ConstantFlowAgreementV1.FlowOperatorData |  |

### _decodeFlowOperatorData

```solidity
function _decodeFlowOperatorData(
    uint256 wordA
) internal returns (bool exist, struct ConstantFlowAgreementV1.FlowOperatorData flowOperatorData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `wordA` | uint256 |  |

### _getBooleanFlowOperatorPermissions

```solidity
function _getBooleanFlowOperatorPermissions(
    uint8 permissions,
    enum ConstantFlowAgreementV1.FlowChangeType flowChangeType
) internal returns (bool flowchangeTypeAllowed)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `permissions` | uint8 |  |
| `flowChangeType` | enum ConstantFlowAgreementV1.FlowChangeType |  |

