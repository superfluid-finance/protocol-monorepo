# Solidity API

## SuperfluidUpgradabilityTester

### constructor

```solidity
constructor() public
```

### validateStorageLayout

```solidity
function validateStorageLayout() external pure
```

### validateContextStructLayout

```solidity
function validateContextStructLayout() external pure
```

## SuperfluidMock

### constructor

```solidity
constructor(bool nonUpgradable, bool appWhiteListingEnabled) public
```

### ctxFunc1

```solidity
function ctxFunc1(uint256 n, bytes ctx) external pure returns (uint256, bytes)
```

### ctxFunc2

```solidity
function ctxFunc2(address superToken, address agreementClass, bytes32 agreementId, bytes agreementData, bytes cbdata, bytes ctx) external pure returns (address, address, bytes32, bytes, bytes, bytes)
```

### testCtxFuncX

```solidity
function testCtxFuncX(bytes dataWithPlaceHolderCtx, bytes ctx) external view returns (bytes returnedData)
```

### jailApp

```solidity
function jailApp(contract ISuperApp app) external
```

