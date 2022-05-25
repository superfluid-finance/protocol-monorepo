# Solidity API

## CustomSuperTokenBaseMock

### getFirstCustomTokenStorageSlot

```solidity
function getFirstCustomTokenStorageSlot() external pure virtual returns (uint256 slot)
```

### callSelfBurn

```solidity
function callSelfBurn(address to, uint256 amount, bytes userData) external virtual
```

### callSelfTransferFrom

```solidity
function callSelfTransferFrom(address holder, address spender, address recipient, uint256 amount) external virtual
```

### callSelfApproveFor

```solidity
function callSelfApproveFor(address account, address spender, uint256 amount) external virtual
```

## CustomSuperTokenMock

## CustomSuperTokenProxyMock

### _firstStorageSlot

```solidity
uint256 _firstStorageSlot
```

### getFirstCustomTokenStorageSlot

```solidity
function getFirstCustomTokenStorageSlot() external pure returns (uint256 slot)
```

### selfMint

```solidity
function selfMint(address to, uint256 amount, bytes userData) external
```

### delegatecallSelfMint

```solidity
function delegatecallSelfMint(address to, uint256 amount, bytes userData) external
```

### callSelfBurn

```solidity
function callSelfBurn(address to, uint256 amount, bytes userData) external
```

### callSelfTransferFrom

```solidity
function callSelfTransferFrom(address holder, address spender, address recipient, uint256 amount) external
```

### callSelfApproveFor

```solidity
function callSelfApproveFor(address account, address spender, uint256 amount) external
```

