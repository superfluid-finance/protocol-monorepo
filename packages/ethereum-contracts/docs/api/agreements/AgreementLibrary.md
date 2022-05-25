# AgreementLibrary

Helper library for building super agreement

## Functions

### authorizeTokenAccess

```solidity
function authorizeTokenAccess(
    contract ISuperfluidToken token,
    bytes ctx
) internal returns (struct ISuperfluid.Context)
```

Authorize the msg.sender to access token agreement storage

NOTE:
- msg.sender must be the expected host contract.
- it should revert on unauthorized access.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `ctx` | bytes |  |

### createCallbackInputs

```solidity
function createCallbackInputs(
    contract ISuperfluidToken token,
    address account,
    bytes32 agreementId,
    bytes agreementData
) internal returns (struct AgreementLibrary.CallbackInputs inputs)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `agreementId` | bytes32 |  |
| `agreementData` | bytes |  |

### callAppBeforeCallback

```solidity
function callAppBeforeCallback(
    struct AgreementLibrary.CallbackInputs inputs,
    bytes ctx
) internal returns (bytes cbdata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `inputs` | struct AgreementLibrary.CallbackInputs |  |
| `ctx` | bytes |  |

### callAppAfterCallback

```solidity
function callAppAfterCallback(
    struct AgreementLibrary.CallbackInputs inputs,
    bytes cbdata,
    bytes ctx
) internal returns (struct ISuperfluid.Context appContext, bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `inputs` | struct AgreementLibrary.CallbackInputs |  |
| `cbdata` | bytes |  |
| `ctx` | bytes |  |

### _selectorFromNoopBit

```solidity
function _selectorFromNoopBit(
    uint256 noopBit
) private returns (bytes4 selector)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `noopBit` | uint256 |  |

### _pushCallbackStack

```solidity
function _pushCallbackStack(
    bytes ctx,
    struct AgreementLibrary.CallbackInputs inputs
) private returns (bytes appCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |
| `inputs` | struct AgreementLibrary.CallbackInputs |  |

### _popCallbackStack

```solidity
function _popCallbackStack(
    bytes ctx,
    int256 appAllowanceUsedDelta
) private returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |
| `appAllowanceUsedDelta` | int256 |  |

### max

```solidity
function max(
    int256 a,
    int256 b
) internal returns (int256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `a` | int256 |  |
| `b` | int256 |  |

### max

```solidity
function max(
    uint256 a,
    uint256 b
) internal returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `a` | uint256 |  |
| `b` | uint256 |  |

### min

```solidity
function min(
    int256 a,
    int256 b
) internal returns (int256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `a` | int256 |  |
| `b` | int256 |  |

