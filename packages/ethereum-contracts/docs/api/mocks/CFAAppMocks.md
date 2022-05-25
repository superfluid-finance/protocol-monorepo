# ExclusiveInflowTestApp

This is a CFA SuperApp that maintains at most one inflow from a sender at any moment.

This can test the deposit allowance logic in the deleteFlow as a recipient.

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

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken ,
    address ,
    bytes32 ,
    bytes agreementData,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `agreementData` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

# NonClosableOutflowTestApp

This is CFA SuperApp that refuses to close its outflow by its receiver.

This test the logic that the app re-opens the same stream in the termination callback.
In reality, the app would have to fund the app with enough tokens to not to be jailed due
to low balance.

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

### setupOutflow

```solidity
function setupOutflow(
    contract ISuperToken superToken,
    address receiver,
    int96 flowRate
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `receiver` | address |  |
| `flowRate` | int96 |  |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken superToken,
    address ,
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
| `` | address |  |
| `` | bytes32 |  |
| `agreementData` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

# SelfDeletingFlowTestApp

This is CFA SuperApp that refuses to accept any opening flow without reverting them.

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

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

# ClosingOnUpdateFlowTestApp

This is CFA SuperApp that closes an updated flow.

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

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken superToken,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

# FlowExchangeTestApp

## Functions

### constructor

```solidity
function constructor(
    contract IConstantFlowAgreementV1 cfa,
    contract ISuperfluid superfluid,
    contract ISuperToken targetToken
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cfa` | contract IConstantFlowAgreementV1 |  |
| `superfluid` | contract ISuperfluid |  |
| `targetToken` | contract ISuperToken |  |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

