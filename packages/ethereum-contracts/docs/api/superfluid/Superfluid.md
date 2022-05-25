# Superfluid

The Superfluid host implementation.

NOTE:
- Please read ISuperfluid for implementation notes.
- For some deeper technical notes, please visit protocol-monorepo wiki area.

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

### initialize

```solidity
function initialize(
    contract ISuperfluidGovernance gov
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `gov` | contract ISuperfluidGovernance |  |

### proxiableUUID

```solidity
function proxiableUUID(
) public returns (bytes32)
```

Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT.

### updateCode

```solidity
function updateCode(
    address newAddress
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newAddress` | address |  |

### getNow

```solidity
function getNow(
) public returns (uint256)
```

### getGovernance

```solidity
function getGovernance(
) external returns (contract ISuperfluidGovernance)
```

Get the current governance address of the Superfluid host

### replaceGovernance

```solidity
function replaceGovernance(
    contract ISuperfluidGovernance newGov
) external
```

Replace the current governance with a new one

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newGov` | contract ISuperfluidGovernance |  |

### registerAgreementClass

```solidity
function registerAgreementClass(
    contract ISuperAgreement agreementClassLogic
) external
```

Register a new agreement class to the system

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClassLogic` | contract ISuperAgreement | Initial agreement class code

Modifiers:
 - onlyGovernance |

### updateAgreementClass

```solidity
function updateAgreementClass(
    contract ISuperAgreement agreementClassLogic
) external
```

Update code of an agreement class

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClassLogic` | contract ISuperAgreement | New code for the agreement class

Modifiers:
 - onlyGovernance |

### isAgreementTypeListed

```solidity
function isAgreementTypeListed(
    bytes32 agreementType
) external returns (bool yes)
```

Check if the agreement type is whitelisted

agreementType is the keccak256 hash of: "org.superfluid-finance.agreements.<AGREEMENT_NAME>.<VERSION>"

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementType` | bytes32 |  |

### isAgreementClassListed

```solidity
function isAgreementClassListed(
    contract ISuperAgreement agreementClass
) public returns (bool yes)
```

Check if the agreement class is whitelisted

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClass` | contract ISuperAgreement |  |

### getAgreementClass

```solidity
function getAgreementClass(
    bytes32 agreementType
) external returns (contract ISuperAgreement agreementClass)
```

Get agreement class

agreementType is the keccak256 hash of: "org.superfluid-finance.agreements.<AGREEMENT_NAME>.<VERSION>"

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementType` | bytes32 |  |

### mapAgreementClasses

```solidity
function mapAgreementClasses(
    uint256 bitmap
) external returns (contract ISuperAgreement[] agreementClasses)
```

Map list of the agreement classes using a bitmap

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `bitmap` | uint256 | Agreement class bitmap |

### addToAgreementClassesBitmap

```solidity
function addToAgreementClassesBitmap(
    uint256 bitmap,
    bytes32 agreementType
) external returns (uint256 newBitmap)
```

Create a new bitmask by adding a agreement class to it

agreementType is the keccak256 hash of: "org.superfluid-finance.agreements.<AGREEMENT_NAME>.<VERSION>"

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `bitmap` | uint256 | Agreement class bitmap |
| `agreementType` | bytes32 |  |

### removeFromAgreementClassesBitmap

```solidity
function removeFromAgreementClassesBitmap(
    uint256 bitmap,
    bytes32 agreementType
) external returns (uint256 newBitmap)
```

Create a new bitmask by removing a agreement class from it

agreementType is the keccak256 hash of: "org.superfluid-finance.agreements.<AGREEMENT_NAME>.<VERSION>"

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `bitmap` | uint256 | Agreement class bitmap |
| `agreementType` | bytes32 |  |

### getSuperTokenFactory

```solidity
function getSuperTokenFactory(
) external returns (contract ISuperTokenFactory factory)
```

Get the super token factory

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `factory` | contract ISuperTokenFactory | The factory |

### getSuperTokenFactoryLogic

```solidity
function getSuperTokenFactoryLogic(
) external returns (address logic)
```

Get the super token factory logic (applicable to upgradable deployment)

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `logic` | address | The factory logic |

### updateSuperTokenFactory

```solidity
function updateSuperTokenFactory(
    contract ISuperTokenFactory newFactory
) external
```

Update super token factory

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newFactory` | contract ISuperTokenFactory | New factory logic |

### updateSuperTokenLogic

```solidity
function updateSuperTokenLogic(
    contract ISuperToken token
) external
```

Update the super token logic to the latest

Refer to ISuperTokenFactory.Upgradability for expected behaviours

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |

### registerApp

```solidity
function registerApp(
    uint256 configWord
) external
```

Message sender (must be a contract) declares itself as a super app.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 | The super app manifest configuration, flags are defined in
`SuperAppDefinitions` |

### registerAppWithKey

```solidity
function registerAppWithKey(
    uint256 configWord,
    string registrationKey
) external
```

While the message sender must be the super app itself, the transaction sender (tx.origin)
must be the deployer account the registration key was issued for.

Message sender declares itself as a super app.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 | The super app manifest configuration, flags are defined in
`SuperAppDefinitions` |
| `registrationKey` | string | The registration key issued by the governance, needed to
register on a mainnet.
See https://github.com/superfluid-finance/protocol-monorepo/wiki/Super-App-White-listing-Guide
On testnets or in dev environment, a placeholder (e.g. empty string) can be used. |

### registerAppByFactory

```solidity
function registerAppByFactory(
    contract ISuperApp app,
    uint256 configWord
) external
```

On mainnet deployments, only factory contracts pre-authorized by governance can use this.
See https://github.com/superfluid-finance/protocol-monorepo/wiki/Super-App-White-listing-Guide

Message sender (must be a contract) declares app as a super app

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `configWord` | uint256 | The super app manifest configuration, flags are defined in
`SuperAppDefinitions` |

### _registerApp

```solidity
function _registerApp(
    uint256 configWord,
    contract ISuperApp app,
    bool checkIfInAppConstructor
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `configWord` | uint256 |  |
| `app` | contract ISuperApp |  |
| `checkIfInAppConstructor` | bool |  |

### isApp

```solidity
function isApp(
    contract ISuperApp app
) public returns (bool)
```

Query if the app is registered

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | Super app address |

### getAppLevel

```solidity
function getAppLevel(
    contract ISuperApp appAddr
) public returns (uint8)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appAddr` | contract ISuperApp |  |

### getAppManifest

```solidity
function getAppManifest(
    contract ISuperApp app
) external returns (bool isSuperApp, bool isJailed, uint256 noopMask)
```

Get the manifest of the super app

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | Super app address |

### isAppJailed

```solidity
function isAppJailed(
    contract ISuperApp app
) public returns (bool)
```

Query if the app has been jailed

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | Super app address |

### allowCompositeApp

```solidity
function allowCompositeApp(
    contract ISuperApp targetApp
) external
```

Whitelist the target app for app composition for the source app (msg.sender)

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `targetApp` | contract ISuperApp | The target super app address |

### isCompositeAppAllowed

```solidity
function isCompositeAppAllowed(
    contract ISuperApp app,
    contract ISuperApp targetApp
) external returns (bool)
```

Query if source app is allowed to call the target app as downstream app

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | Super app address |
| `targetApp` | contract ISuperApp | The target super app address |

### callAppBeforeCallback

```solidity
function callAppBeforeCallback(
    contract ISuperApp app,
    bytes callData,
    bool isTermination,
    bytes ctx
) external returns (bytes cbdata)
```

(For agreements) StaticCall the app before callback

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | The super app. |
| `callData` | bytes | The call data sending to the super app. |
| `isTermination` | bool | Is it a termination callback? |
| `ctx` | bytes | Current ctx, it will be validated. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `cbdata` | bytes | Data returned from the callback. |

### callAppAfterCallback

```solidity
function callAppAfterCallback(
    contract ISuperApp app,
    bytes callData,
    bool isTermination,
    bytes ctx
) external returns (bytes newCtx)
```

(For agreements) Call the app after callback

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp | The super app. |
| `callData` | bytes | The call data sending to the super app. |
| `isTermination` | bool | Is it a termination callback? |
| `ctx` | bytes | Current ctx, it will be validated. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction. |

### appCallbackPush

```solidity
function appCallbackPush(
    bytes ctx,
    contract ISuperApp app,
    uint256 appAllowanceGranted,
    int256 appAllowanceUsed,
    contract ISuperfluidToken appAllowanceToken
) external returns (bytes appCtx)
```

(For agreements) Create a new callback stack

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes | The current ctx, it will be validated. |
| `app` | contract ISuperApp | The super app. |
| `appAllowanceGranted` | uint256 | App allowance granted so far. |
| `appAllowanceUsed` | int256 | App allowance used so far. |
| `appAllowanceToken` | contract ISuperfluidToken |  |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `appCtx` | bytes |  |

### appCallbackPop

```solidity
function appCallbackPop(
    bytes ctx,
    int256 appAllowanceUsedDelta
) external returns (bytes newCtx)
```

(For agreements) Pop from the current app callback stack

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes | The ctx that was pushed before the callback stack. |
| `appAllowanceUsedDelta` | int256 | App allowance used by the app. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction.

[SECURITY] NOTE:
- Here we cannot do assertValidCtx(ctx), since we do not really save the stack in memory.
- Hence there is still implicit trust that the agreement handles the callback push/pop pair correctly. |

### ctxUseAllowance

```solidity
function ctxUseAllowance(
    bytes ctx,
    uint256 appAllowanceWantedMore,
    int256 appAllowanceUsedDelta
) external returns (bytes newCtx)
```

(For agreements) Use app allowance.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes | The current ctx, it will be validated. |
| `appAllowanceWantedMore` | uint256 | See app allowance for more details. |
| `appAllowanceUsedDelta` | int256 | See app allowance for more details. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction. |

### jailApp

```solidity
function jailApp(
    bytes ctx,
    contract ISuperApp app,
    uint256 reason
) external returns (bytes newCtx)
```

(For agreements) Jail the app.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |
| `app` | contract ISuperApp | The super app. |
| `reason` | uint256 | Jail reason code. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newCtx` | bytes | The current context of the transaction. |

### _callAgreement

```solidity
function _callAgreement(
    address msgSender,
    contract ISuperAgreement agreementClass,
    bytes callData,
    bytes userData
) internal returns (bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `msgSender` | address |  |
| `agreementClass` | contract ISuperAgreement |  |
| `callData` | bytes |  |
| `userData` | bytes |  |

### callAgreement

```solidity
function callAgreement(
    contract ISuperAgreement agreementClass,
    bytes callData,
    bytes userData
) external returns (bytes returnedData)
```

Call agreement function

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClass` | contract ISuperAgreement | The agreement address you are calling |
| `callData` | bytes | The contextual call data with placeholder ctx |
| `userData` | bytes | Extra user data being sent to the super app callbacks |

### _callAppAction

```solidity
function _callAppAction(
    address msgSender,
    contract ISuperApp app,
    bytes callData
) internal returns (bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `msgSender` | address |  |
| `app` | contract ISuperApp |  |
| `callData` | bytes |  |

### callAppAction

```solidity
function callAppAction(
    contract ISuperApp app,
    bytes callData
) external returns (bytes returnedData)
```

Call app action

Main use case is calling app action in a batch call via the host

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `callData` | bytes | The contextual call data

NOTE: See "Contextless Call Proxies" above for more about contextual call data. |

### callAgreementWithContext

```solidity
function callAgreementWithContext(
    contract ISuperAgreement agreementClass,
    bytes callData,
    bytes userData,
    bytes ctx
) external returns (bytes newCtx, bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `agreementClass` | contract ISuperAgreement |  |
| `callData` | bytes |  |
| `userData` | bytes |  |
| `ctx` | bytes |  |

### callAppActionWithContext

```solidity
function callAppActionWithContext(
    contract ISuperApp app,
    bytes callData,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `callData` | bytes |  |
| `ctx` | bytes |  |

### decodeCtx

```solidity
function decodeCtx(
    bytes ctx
) public returns (struct ISuperfluid.Context context)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### isCtxValid

```solidity
function isCtxValid(
    bytes ctx
) external returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### _batchCall

```solidity
function _batchCall(
    address msgSender,
    struct ISuperfluid.Operation[] operations
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `msgSender` | address |  |
| `operations` | struct ISuperfluid.Operation[] |  |

### batchCall

```solidity
function batchCall(
    struct ISuperfluid.Operation[] operations
) external
```

ISuperfluid.batchCall implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operations` | struct ISuperfluid.Operation[] |  |

### forwardBatchCall

```solidity
function forwardBatchCall(
    struct ISuperfluid.Operation[] operations
) external
```

ISuperfluid.forwardBatchCall implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `operations` | struct ISuperfluid.Operation[] |  |

### isTrustedForwarder

```solidity
function isTrustedForwarder(
    address forwarder
) public returns (bool)
```

BaseRelayRecipient.isTrustedForwarder implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `forwarder` | address |  |

### versionRecipient

```solidity
function versionRecipient(
) external returns (string)
```

IRelayRecipient.isTrustedForwarder implementation

### _jailApp

```solidity
function _jailApp(
    contract ISuperApp app,
    uint256 reason
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `reason` | uint256 |  |

### _updateContext

```solidity
function _updateContext(
    struct ISuperfluid.Context context
) private returns (bytes ctx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `context` | struct ISuperfluid.Context |  |

### _decodeCtx

```solidity
function _decodeCtx(
    bytes ctx
) private returns (struct ISuperfluid.Context context)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### _isCtxValid

```solidity
function _isCtxValid(
    bytes ctx
) private returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `ctx` | bytes |  |

### _callExternalWithReplacedCtx

```solidity
function _callExternalWithReplacedCtx(
    address target,
    bytes callData,
    bytes ctx
) private returns (bool success, bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `target` | address |  |
| `callData` | bytes |  |
| `ctx` | bytes |  |

### _callCallback

```solidity
function _callCallback(
    contract ISuperApp app,
    bool isStaticall,
    bool isTermination,
    bytes callData,
    bytes ctx
) private returns (bool success, bytes returnedData)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `app` | contract ISuperApp |  |
| `isStaticall` | bool |  |
| `isTermination` | bool |  |
| `callData` | bytes |  |
| `ctx` | bytes |  |

### _replacePlaceholderCtx

```solidity
function _replacePlaceholderCtx(
    bytes data,
    bytes ctx
) internal returns (bytes dataWithCtx)
```

Replace the placeholder ctx with the actual ctx

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `data` | bytes |  |
| `ctx` | bytes |  |

