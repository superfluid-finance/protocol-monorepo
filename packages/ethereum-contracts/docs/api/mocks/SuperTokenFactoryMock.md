# SuperTokenFactoryStorageLayoutTester

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

### validateStorageLayout

```solidity
function validateStorageLayout(
) external
```

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(
    contract ISuperfluid 
) external returns (address)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `` | contract ISuperfluid |  |

# SuperTokenFactoryMockHelper

## Functions

### create

```solidity
function create(
    contract ISuperfluid host,
    uint256 waterMark
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `waterMark` | uint256 |  |

# SuperTokenFactoryMock

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    contract SuperTokenFactoryMockHelper helper
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `helper` | contract SuperTokenFactoryMockHelper |  |

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(
    contract ISuperfluid host
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

# SuperTokenFactoryMock42

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    contract SuperTokenFactoryMockHelper helper
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `helper` | contract SuperTokenFactoryMockHelper |  |

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(
    contract ISuperfluid host
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

