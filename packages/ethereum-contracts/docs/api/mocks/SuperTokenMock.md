# Solidity API

## SuperTokenStorageLayoutTester

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### validateStorageLayout

```solidity
function validateStorageLayout() external pure
```

### getLastSuperTokenStorageSlot

```solidity
function getLastSuperTokenStorageSlot() external pure returns (uint256 slot)
```

## SuperTokenMock

### waterMark

```solidity
uint256 waterMark
```

### constructor

```solidity
constructor(contract ISuperfluid host, uint256 w) public
```

### approveInternal

```solidity
function approveInternal(address owner, address spender, uint256 value) external
```

ERC-20 mockings

### transferInternal

```solidity
function transferInternal(address from, address to, uint256 value) external
```

### setupDefaultOperators

```solidity
function setupDefaultOperators(address[] operators) external
```

ERC-777 mockings

### mintInternal

```solidity
function mintInternal(address to, uint256 amount, bytes userData, bytes operatorData) external
```

