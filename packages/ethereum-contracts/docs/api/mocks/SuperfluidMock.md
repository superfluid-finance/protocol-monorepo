# SuperfluidUpgradabilityTester

## Functions

### constructor

```solidity
function constructor(
) public
```

### validateStorageLayout

```solidity
function validateStorageLayout(
) external
```

### validateContextStructLayout

```solidity
function validateContextStructLayout(
) external
```

# SuperfluidMock

## Functions

### constructor

```solidity
function constructor(
    bool nonUpgradable,
    bool appWhiteListingEnabled
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `nonUpgradable` | bool |  |
| `appWhiteListingEnabled` | bool |  |

### ctxFunc1

```solidity
function ctxFunc1(
    uint256 n,
    bytes ctx
) external returns (uint256, bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `n` | uint256 |  |
| `ctx` | bytes |  |

### ctxFunc2

```solidity
function ctxFunc2(
    address superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (address, address, bytes32, bytes, bytes, bytes)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | address |  |
| `agreementClass` | address |  |
| `agreementId` | bytes32 |  |
| `agreementData` | bytes |  |
| `cbdata` | bytes |  |
| `ctx` | bytes |  |

### testCtxFuncX

```solidity
function testCtxFuncX(
    bytes dataWithPlaceHolderCtx,
    bytes ctx
) external returns (bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `dataWithPlaceHolderCtx` | bytes |  |
| `ctx` | bytes |  |

### jailApp

```solidity
function jailApp(
    contract ISuperApp app
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |

