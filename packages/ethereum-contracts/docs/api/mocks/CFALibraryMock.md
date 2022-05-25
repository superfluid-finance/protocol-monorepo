# Solidity API

## CFALibraryMock

### cfaV1

```solidity
struct CFAv1Library.InitData cfaV1
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### createFlowTest

```solidity
function createFlowTest(contract ISuperfluidToken token, address receiver, int96 flowRate) public
```

### updateFlowTest

```solidity
function updateFlowTest(contract ISuperfluidToken token, address receiver, int96 flowRate) public
```

### deleteFlowTest

```solidity
function deleteFlowTest(contract ISuperfluidToken token, address receiver) public
```

### createFlowByOperatorTest

```solidity
function createFlowByOperatorTest(address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) public
```

### updateFlowByOperatorTest

```solidity
function updateFlowByOperatorTest(address sender, address receiver, contract ISuperfluidToken token, int96 flowRate) public
```

### deleteFlowByOperator

```solidity
function deleteFlowByOperator(address sender, address receiver, contract ISuperfluidToken token) public
```

### updateFlowOperatorPermissionsTest

```solidity
function updateFlowOperatorPermissionsTest(address flowOperator, contract ISuperfluidToken token, uint8 permissions, int96 flowRateAllowance) public
```

### authorizeFlowOperatorWithFullControlTest

```solidity
function authorizeFlowOperatorWithFullControlTest(address flowOperator, contract ISuperfluidToken token) public
```

### revokeFlowOperatorWithFullControlTest

```solidity
function revokeFlowOperatorWithFullControlTest(address flowOperator, contract ISuperfluidToken token) public
```

## CFALibrarySuperAppMock

### cfaV1

```solidity
struct CFAv1Library.InitData cfaV1
```

### permissions

```solidity
uint8 permissions
```

### flowRate

```solidity
int96 flowRate
```

### updatedFlowRate

```solidity
int96 updatedFlowRate
```

### sender

```solidity
address sender
```

### receiver

```solidity
address receiver
```

### flowOperator

```solidity
address flowOperator
```

### FunctionIndex

```solidity
enum FunctionIndex {
  CREATE_FLOW,
  UPDATE_FLOW,
  DELETE_FLOW,
  CREATE_FLOW_BY_OPERATOR,
  UPDATE_FLOW_BY_OPERATOR,
  DELETE_FLOW_BY_OPERATOR,
  UPDATE_FLOW_OPERATOR_PERMISSIONS,
  AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL,
  REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL
}
```

### constructor

```solidity
constructor(contract ISuperfluid host, address defaultSender, address defaultReceiver, address defaultFlowOperator) public
```

### createFlow

```solidity
function createFlow(contract ISuperToken token) external
```

### authorizeFlowOperatorWithFullControl

```solidity
function authorizeFlowOperatorWithFullControl(contract ISuperToken token) external
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken token, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes)
```

