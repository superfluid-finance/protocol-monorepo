# ISuperApp

Be aware of the app being jailed, when the word permitted is used.

## Functions

### beforeAgreementCreated

```solidity
function beforeAgreementCreated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes cbdata)
```

Callback before a new agreement is created.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes | A free format in memory data the app can use to pass
         arbitary information to the after-hook callback.

NOTE:
- It will be invoked with `staticcall`, no state changes are permitted.
- Only revert with a "reason" is permitted. |

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is created.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Only revert with a "reason" is permitted. |

### beforeAgreementUpdated

```solidity
function beforeAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes cbdata)
```

Callback before a new agreement is updated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes | A free format in memory data the app can use to pass
         arbitary information to the after-hook callback.

NOTE:
- It will be invoked with `staticcall`, no state changes are permitted.
- Only revert with a "reason" is permitted. |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is updated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Only revert with a "reason" is permitted. |

### beforeAgreementTerminated

```solidity
function beforeAgreementTerminated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes ctx
) external returns (bytes cbdata)
```

Callback before a new agreement is terminated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes | A free format in memory data the app can use to pass
         arbitary information to the after-hook callback.

NOTE:
- It will be invoked with `staticcall`, no state changes are permitted.
- Revert is not permitted. |

### afterAgreementTerminated

```solidity
function afterAgreementTerminated(
    contract ISuperToken superToken,
    address agreementClass,
    bytes32 agreementId,
    bytes agreementData,
    bytes cbdata,
    bytes ctx
) external returns (bytes newCtx)
```

Callback after a new agreement is terminated.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `superToken` | contract ISuperToken | The super token used for the agreement. |
| `agreementClass` | address | The agreement class address. |
| `agreementId` | bytes32 | The agreementId |
| `agreementData` | bytes | The agreement data (non-compressed) |
| `cbdata` | bytes | The data returned from the before-hook callback. |
| `ctx` | bytes | The context data. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

NOTE:
- State changes is permitted.
- Revert is not permitted. |

