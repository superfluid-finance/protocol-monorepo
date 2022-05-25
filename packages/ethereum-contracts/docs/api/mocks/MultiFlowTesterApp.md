# MultiFlowTesterApp

A super app that can split incoming flows to multiple outgoing flows.
     This is used for testing CFA callbacks logic.

## Functions

### constructor

```solidity
function constructor(
    contract IConstantFlowAgreementV1 cfa,
    contract ISuperfluid superfluid
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cfa` | contract IConstantFlowAgreementV1 |  |
| `superfluid` | contract ISuperfluid |  |

### _parseUserData

```solidity
function _parseUserData(
    bytes userData
) private returns (address sender, struct MultiFlowTesterApp.Configuration configuration)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `userData` | bytes |  |

### _sumProportions

```solidity
function _sumProportions(
    struct MultiFlowTesterApp.ReceiverData[] receivers
) internal returns (uint256 sum)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `receivers` | struct MultiFlowTesterApp.ReceiverData[] |  |

### _updateMultiFlow

```solidity
function _updateMultiFlow(
    struct MultiFlowTesterApp.Configuration configuration,
    contract ISuperToken superToken,
    bytes4 selector,
    int96 flowRate,
    uint256 appAllowanceGranted,
    bytes ctx
) private returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configuration` | struct MultiFlowTesterApp.Configuration |  |
| `superToken` | contract ISuperToken |  |
| `selector` | bytes4 |  |
| `flowRate` | int96 |  |
| `appAllowanceGranted` | uint256 |  |
| `ctx` | bytes |  |

### createFlow

```solidity
function createFlow(
    contract ISuperToken superToken,
    address receiver,
    int96 flowRate,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |
| `ctx` | bytes |  |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `agreementClass` | address |  |
| `agreementId` | bytes32 |  |
| `agreementData` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes ,
    bytes 
) external returns (bytes cbdata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `agreementClass` | address |  |
| `agreementId` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `agreementClass` | address |  |
| `agreementId` | bytes32 |  |
| `agreementData` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 ,
    bytes agreementData,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `agreementClass` | address |  |
| `` | bytes32 |  |
| `agreementData` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

