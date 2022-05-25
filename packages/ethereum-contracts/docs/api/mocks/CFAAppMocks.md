# Solidity API

## ExclusiveInflowTestApp

_This is a CFA SuperApp that maintains at most one inflow from a sender at any moment.

This can test the deposit allowance logic in the deleteFlow as a recipient._

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### _currentSender

```solidity
address _currentSender
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid) public
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken superToken, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken, address, bytes32, bytes agreementData, bytes, bytes ctx) external returns (bytes newCtx)
```

## NonClosableOutflowTestApp

_This is CFA SuperApp that refuses to close its outflow by its receiver.

This test the logic that the app re-opens the same stream in the termination callback.
In reality, the app would have to fund the app with enough tokens to not to be jailed due
to low balance._

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### _receiver

```solidity
address _receiver
```

### _flowRate

```solidity
int96 _flowRate
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid) public
```

### setupOutflow

```solidity
function setupOutflow(contract ISuperToken superToken, address receiver, int96 flowRate) external
```

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(contract ISuperToken superToken, address, bytes32, bytes agreementData, bytes, bytes ctx) external returns (bytes newCtx)
```

## SelfDeletingFlowTestApp

_This is CFA SuperApp that refuses to accept any opening flow without reverting them._

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### _receiver

```solidity
address _receiver
```

### _flowRate

```solidity
int96 _flowRate
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid) public
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken superToken, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

## ClosingOnUpdateFlowTestApp

_This is CFA SuperApp that closes an updated flow._

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid) public
```

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken superToken, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

## FlowExchangeTestApp

### _cfa

```solidity
contract IConstantFlowAgreementV1 _cfa
```

### _host

```solidity
contract ISuperfluid _host
```

### _targetToken

```solidity
contract ISuperToken _targetToken
```

### constructor

```solidity
constructor(contract IConstantFlowAgreementV1 cfa, contract ISuperfluid superfluid, contract ISuperToken targetToken) public
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken superToken, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

