# CFALibraryMock

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

### createFlowTest

```solidity
function createFlowTest(
    contract ISuperfluidToken token,
    address receiver,
    int96 flowRate
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |

### updateFlowTest

```solidity
function updateFlowTest(
    contract ISuperfluidToken token,
    address receiver,
    int96 flowRate
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |

### deleteFlowTest

```solidity
function deleteFlowTest(
    contract ISuperfluidToken token,
    address receiver
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `receiver` | address |  |

### createFlowByOperatorTest

```solidity
function createFlowByOperatorTest(
    address sender,
    address receiver,
    contract ISuperfluidToken token,
    int96 flowRate
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `receiver` | address |  |
| `token` | contract ISuperfluidToken |  |
| `flowRate` | int96 |  |

### updateFlowByOperatorTest

```solidity
function updateFlowByOperatorTest(
    address sender,
    address receiver,
    contract ISuperfluidToken token,
    int96 flowRate
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `receiver` | address |  |
| `token` | contract ISuperfluidToken |  |
| `flowRate` | int96 |  |

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(
    address sender,
    address receiver,
    contract ISuperfluidToken token
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sender` | address |  |
| `receiver` | address |  |
| `token` | contract ISuperfluidToken |  |

### updateFlowOperatorPermissionsTest

```solidity
function updateFlowOperatorPermissionsTest(
    address flowOperator,
    contract ISuperfluidToken token,
    uint8 permissions,
    int96 flowRateAllowance
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowOperator` | address |  |
| `token` | contract ISuperfluidToken |  |
| `permissions` | uint8 |  |
| `flowRateAllowance` | int96 |  |

### authorizeFlowOperatorWithFullControlTest

```solidity
function authorizeFlowOperatorWithFullControlTest(
    address flowOperator,
    contract ISuperfluidToken token
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowOperator` | address |  |
| `token` | contract ISuperfluidToken |  |

### revokeFlowOperatorWithFullControlTest

```solidity
function revokeFlowOperatorWithFullControlTest(
    address flowOperator,
    contract ISuperfluidToken token
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `flowOperator` | address |  |
| `token` | contract ISuperfluidToken |  |

# CFALibrarySuperAppMock

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    address defaultSender,
    address defaultReceiver,
    address defaultFlowOperator
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `defaultSender` | address |  |
| `defaultReceiver` | address |  |
| `defaultFlowOperator` | address |  |

### createFlow

```solidity
function createFlow(
    contract ISuperToken token
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(
    contract ISuperToken token
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken token,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

