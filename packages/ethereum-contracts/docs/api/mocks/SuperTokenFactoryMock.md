# Solidity API

## SuperTokenFactoryStorageLayoutTester

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### validateStorageLayout

```solidity
function validateStorageLayout() external pure
```

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(contract ISuperfluid) external pure returns (address)
```

## SuperTokenFactoryMockHelper

### create

```solidity
function create(contract ISuperfluid host, uint256 waterMark) external returns (address logic)
```

## SuperTokenFactoryMock

### _helper

```solidity
contract SuperTokenFactoryMockHelper _helper
```

### constructor

```solidity
constructor(contract ISuperfluid host, contract SuperTokenFactoryMockHelper helper) public
```

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(contract ISuperfluid host) external returns (address logic)
```

## SuperTokenFactoryMock42

### _helper

```solidity
contract SuperTokenFactoryMockHelper _helper
```

### constructor

```solidity
constructor(contract ISuperfluid host, contract SuperTokenFactoryMockHelper helper) public
```

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(contract ISuperfluid host) external returns (address logic)
```

