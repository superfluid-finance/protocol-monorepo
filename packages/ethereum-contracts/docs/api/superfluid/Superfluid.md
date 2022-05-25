# Solidity API

## Superfluid

_The Superfluid host implementation.

NOTE:
- Please read ISuperfluid for implementation notes.
- For some deeper technical notes, please visit protocol-monorepo wiki area._

### AppManifest

```solidity
struct AppManifest {
  uint256 configWord;
}
```

### NON_UPGRADABLE_DEPLOYMENT

```solidity
bool NON_UPGRADABLE_DEPLOYMENT
```

### APP_WHITE_LISTING_ENABLED

```solidity
bool APP_WHITE_LISTING_ENABLED
```

### MAX_APP_LEVEL

```solidity
uint256 MAX_APP_LEVEL
```

_Maximum number of level of apps can be composed together

NOTE:
- TODO Composite app feature is currently disabled. Hence app cannot
  will not be able to call other app._

### CALLBACK_GAS_LIMIT

```solidity
uint64 CALLBACK_GAS_LIMIT
```

### _gov

```solidity
contract ISuperfluidGovernance _gov
```

_Governance contract_

### _agreementClasses

```solidity
contract ISuperAgreement[] _agreementClasses
```

_Agreement list indexed by agreement index minus one_

### _agreementClassIndices

```solidity
mapping(bytes32 &#x3D;&gt; uint256) _agreementClassIndices
```

_Mapping between agreement type to agreement index (starting from 1)_

### _superTokenFactory

```solidity
contract ISuperTokenFactory _superTokenFactory
```

_Super token_

### _appManifests

```solidity
mapping(contract ISuperApp &#x3D;&gt; struct Superfluid.AppManifest) _appManifests
```

_App manifests_

### _compositeApps

```solidity
mapping(contract ISuperApp &#x3D;&gt; mapping(contract ISuperApp &#x3D;&gt; bool)) _compositeApps
```

_Composite app white-listing: source app &#x3D;&gt; (target app &#x3D;&gt; isAllowed)_

### _ctxStamp

```solidity
bytes32 _ctxStamp
```

_Ctx stamp of the current transaction, it should always be cleared to
     zero before transaction finishes_

### _appKeysUsedDeprecated

```solidity
mapping(bytes32 &#x3D;&gt; bool) _appKeysUsedDeprecated
```

_if app whitelisting is enabled, this is to make sure the keys are used only once_

### constructor

```solidity
constructor(bool nonUpgradable, bool appWhiteListingEnabled) public
```

### initialize

```solidity
function initialize(contract ISuperfluidGovernance gov) external
```

### proxiableUUID

```solidity
function proxiableUUID() public pure returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### updateCode

```solidity
function updateCode(address newAddress) external
```

### getNow

```solidity
function getNow() public view returns (uint256)
```

### getGovernance

```solidity
function getGovernance() external view returns (contract ISuperfluidGovernance)
```

_Get the current governance address of the Superfluid host_

### replaceGovernance

```solidity
function replaceGovernance(contract ISuperfluidGovernance newGov) external
```

_Replace the current governance with a new one_

### registerAgreementClass

```solidity
function registerAgreementClass(contract ISuperAgreement agreementClassLogic) external
```

_Register a new agreement class to the system_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementClassLogic | contract ISuperAgreement | Initial agreement class code Modifiers:  - onlyGovernance |

### updateAgreementClass

```solidity
function updateAgreementClass(contract ISuperAgreement agreementClassLogic) external
```

_Update code of an agreement class_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementClassLogic | contract ISuperAgreement | New code for the agreement class Modifiers:  - onlyGovernance |

### isAgreementTypeListed

```solidity
function isAgreementTypeListed(bytes32 agreementType) external view returns (bool yes)
```

Check if the agreement type is whitelisted

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

### isAgreementClassListed

```solidity
function isAgreementClassListed(contract ISuperAgreement agreementClass) public view returns (bool yes)
```

_Check if the agreement class is whitelisted_

### getAgreementClass

```solidity
function getAgreementClass(bytes32 agreementType) external view returns (contract ISuperAgreement agreementClass)
```

Get agreement class

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

### mapAgreementClasses

```solidity
function mapAgreementClasses(uint256 bitmap) external view returns (contract ISuperAgreement[] agreementClasses)
```

_Map list of the agreement classes using a bitmap_

| Name | Type | Description |
| ---- | ---- | ----------- |
| bitmap | uint256 | Agreement class bitmap |

### addToAgreementClassesBitmap

```solidity
function addToAgreementClassesBitmap(uint256 bitmap, bytes32 agreementType) external view returns (uint256 newBitmap)
```

Create a new bitmask by adding a agreement class to it

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| bitmap | uint256 | Agreement class bitmap |
| agreementType | bytes32 |  |

### removeFromAgreementClassesBitmap

```solidity
function removeFromAgreementClassesBitmap(uint256 bitmap, bytes32 agreementType) external view returns (uint256 newBitmap)
```

Create a new bitmask by removing a agreement class from it

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| bitmap | uint256 | Agreement class bitmap |
| agreementType | bytes32 |  |

### getSuperTokenFactory

```solidity
function getSuperTokenFactory() external view returns (contract ISuperTokenFactory factory)
```

_Get the super token factory_

| Name | Type | Description |
| ---- | ---- | ----------- |
| factory | contract ISuperTokenFactory | The factory |

### getSuperTokenFactoryLogic

```solidity
function getSuperTokenFactoryLogic() external view returns (address logic)
```

_Get the super token factory logic (applicable to upgradable deployment)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| logic | address | The factory logic |

### updateSuperTokenFactory

```solidity
function updateSuperTokenFactory(contract ISuperTokenFactory newFactory) external
```

_Update super token factory_

| Name | Type | Description |
| ---- | ---- | ----------- |
| newFactory | contract ISuperTokenFactory | New factory logic |

### updateSuperTokenLogic

```solidity
function updateSuperTokenLogic(contract ISuperToken token) external
```

Update the super token logic to the latest

_Refer to ISuperTokenFactory.Upgradability for expected behaviours_

### registerApp

```solidity
function registerApp(uint256 configWord) external
```

_Message sender (must be a contract) declares itself as a super app._

| Name | Type | Description |
| ---- | ---- | ----------- |
| configWord | uint256 | The super app manifest configuration, flags are defined in &#x60;SuperAppDefinitions&#x60; |

### registerAppWithKey

```solidity
function registerAppWithKey(uint256 configWord, string registrationKey) external
```

While the message sender must be the super app itself, the transaction sender (tx.origin)
must be the deployer account the registration key was issued for.

_Message sender declares itself as a super app._

| Name | Type | Description |
| ---- | ---- | ----------- |
| configWord | uint256 | The super app manifest configuration, flags are defined in &#x60;SuperAppDefinitions&#x60; |
| registrationKey | string | The registration key issued by the governance, needed to register on a mainnet. See https://github.com/superfluid-finance/protocol-monorepo/wiki/Super-App-White-listing-Guide On testnets or in dev environment, a placeholder (e.g. empty string) can be used. |

### registerAppByFactory

```solidity
function registerAppByFactory(contract ISuperApp app, uint256 configWord) external
```

On mainnet deployments, only factory contracts pre-authorized by governance can use this.
See https://github.com/superfluid-finance/protocol-monorepo/wiki/Super-App-White-listing-Guide

_Message sender (must be a contract) declares app as a super app_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp |  |
| configWord | uint256 | The super app manifest configuration, flags are defined in &#x60;SuperAppDefinitions&#x60; |

### _registerApp

```solidity
function _registerApp(uint256 configWord, contract ISuperApp app, bool checkIfInAppConstructor) private
```

### isApp

```solidity
function isApp(contract ISuperApp app) public view returns (bool)
```

_Query if the app is registered_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |

### getAppLevel

```solidity
function getAppLevel(contract ISuperApp appAddr) public view returns (uint8)
```

### getAppManifest

```solidity
function getAppManifest(contract ISuperApp app) external view returns (bool isSuperApp, bool isJailed, uint256 noopMask)
```

_Get the manifest of the super app_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |

### isAppJailed

```solidity
function isAppJailed(contract ISuperApp app) public view returns (bool)
```

_Query if the app has been jailed_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |

### allowCompositeApp

```solidity
function allowCompositeApp(contract ISuperApp targetApp) external
```

_Whitelist the target app for app composition for the source app (msg.sender)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| targetApp | contract ISuperApp | The target super app address |

### isCompositeAppAllowed

```solidity
function isCompositeAppAllowed(contract ISuperApp app, contract ISuperApp targetApp) external view returns (bool)
```

_Query if source app is allowed to call the target app as downstream app_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |
| targetApp | contract ISuperApp | The target super app address |

### callAppBeforeCallback

```solidity
function callAppBeforeCallback(contract ISuperApp app, bytes callData, bool isTermination, bytes ctx) external returns (bytes cbdata)
```

_(For agreements) StaticCall the app before callback_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | The super app. |
| callData | bytes | The call data sending to the super app. |
| isTermination | bool | Is it a termination callback? |
| ctx | bytes | Current ctx, it will be validated. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| cbdata | bytes | Data returned from the callback. |

### callAppAfterCallback

```solidity
function callAppAfterCallback(contract ISuperApp app, bytes callData, bool isTermination, bytes ctx) external returns (bytes newCtx)
```

_(For agreements) Call the app after callback_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | The super app. |
| callData | bytes | The call data sending to the super app. |
| isTermination | bool | Is it a termination callback? |
| ctx | bytes | Current ctx, it will be validated. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. |

### appCallbackPush

```solidity
function appCallbackPush(bytes ctx, contract ISuperApp app, uint256 appAllowanceGranted, int256 appAllowanceUsed, contract ISuperfluidToken appAllowanceToken) external returns (bytes appCtx)
```

_(For agreements) Create a new callback stack_

| Name | Type | Description |
| ---- | ---- | ----------- |
| ctx | bytes | The current ctx, it will be validated. |
| app | contract ISuperApp | The super app. |
| appAllowanceGranted | uint256 | App allowance granted so far. |
| appAllowanceUsed | int256 | App allowance used so far. |
| appAllowanceToken | contract ISuperfluidToken |  |

| Name | Type | Description |
| ---- | ---- | ----------- |
| appCtx | bytes |  |

### appCallbackPop

```solidity
function appCallbackPop(bytes ctx, int256 appAllowanceUsedDelta) external returns (bytes newCtx)
```

_(For agreements) Pop from the current app callback stack_

| Name | Type | Description |
| ---- | ---- | ----------- |
| ctx | bytes | The ctx that was pushed before the callback stack. |
| appAllowanceUsedDelta | int256 | App allowance used by the app. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. [SECURITY] NOTE: - Here we cannot do assertValidCtx(ctx), since we do not really save the stack in memory. - Hence there is still implicit trust that the agreement handles the callback push/pop pair correctly. |

### ctxUseAllowance

```solidity
function ctxUseAllowance(bytes ctx, uint256 appAllowanceWantedMore, int256 appAllowanceUsedDelta) external returns (bytes newCtx)
```

_(For agreements) Use app allowance._

| Name | Type | Description |
| ---- | ---- | ----------- |
| ctx | bytes | The current ctx, it will be validated. |
| appAllowanceWantedMore | uint256 | See app allowance for more details. |
| appAllowanceUsedDelta | int256 | See app allowance for more details. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. |

### jailApp

```solidity
function jailApp(bytes ctx, contract ISuperApp app, uint256 reason) external returns (bytes newCtx)
```

_(For agreements) Jail the app._

| Name | Type | Description |
| ---- | ---- | ----------- |
| ctx | bytes |  |
| app | contract ISuperApp | The super app. |
| reason | uint256 | Jail reason code. |

| Name | Type | Description |
| ---- | ---- | ----------- |
| newCtx | bytes | The current context of the transaction. |

### _callAgreement

```solidity
function _callAgreement(address msgSender, contract ISuperAgreement agreementClass, bytes callData, bytes userData) internal returns (bytes returnedData)
```

### callAgreement

```solidity
function callAgreement(contract ISuperAgreement agreementClass, bytes callData, bytes userData) external returns (bytes returnedData)
```

_Call agreement function_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementClass | contract ISuperAgreement | The agreement address you are calling |
| callData | bytes | The contextual call data with placeholder ctx |
| userData | bytes | Extra user data being sent to the super app callbacks |

### _callAppAction

```solidity
function _callAppAction(address msgSender, contract ISuperApp app, bytes callData) internal returns (bytes returnedData)
```

### callAppAction

```solidity
function callAppAction(contract ISuperApp app, bytes callData) external returns (bytes returnedData)
```

Call app action

_Main use case is calling app action in a batch call via the host_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp |  |
| callData | bytes | The contextual call data NOTE: See &quot;Contextless Call Proxies&quot; above for more about contextual call data. |

### callAgreementWithContext

```solidity
function callAgreementWithContext(contract ISuperAgreement agreementClass, bytes callData, bytes userData, bytes ctx) external returns (bytes newCtx, bytes returnedData)
```

### callAppActionWithContext

```solidity
function callAppActionWithContext(contract ISuperApp app, bytes callData, bytes ctx) external returns (bytes newCtx)
```

### decodeCtx

```solidity
function decodeCtx(bytes ctx) public pure returns (struct ISuperfluid.Context context)
```

### isCtxValid

```solidity
function isCtxValid(bytes ctx) external view returns (bool)
```

### _batchCall

```solidity
function _batchCall(address msgSender, struct ISuperfluid.Operation[] operations) internal
```

### batchCall

```solidity
function batchCall(struct ISuperfluid.Operation[] operations) external
```

_ISuperfluid.batchCall implementation_

### forwardBatchCall

```solidity
function forwardBatchCall(struct ISuperfluid.Operation[] operations) external
```

_ISuperfluid.forwardBatchCall implementation_

### isTrustedForwarder

```solidity
function isTrustedForwarder(address forwarder) public view returns (bool)
```

_BaseRelayRecipient.isTrustedForwarder implementation_

### versionRecipient

```solidity
function versionRecipient() external pure returns (string)
```

_IRelayRecipient.isTrustedForwarder implementation_

### _jailApp

```solidity
function _jailApp(contract ISuperApp app, uint256 reason) internal
```

### _updateContext

```solidity
function _updateContext(struct ISuperfluid.Context context) private returns (bytes ctx)
```

### _decodeCtx

```solidity
function _decodeCtx(bytes ctx) private pure returns (struct ISuperfluid.Context context)
```

### _isCtxValid

```solidity
function _isCtxValid(bytes ctx) private view returns (bool)
```

### _callExternalWithReplacedCtx

```solidity
function _callExternalWithReplacedCtx(address target, bytes callData, bytes ctx) private returns (bool success, bytes returnedData)
```

### _callCallback

```solidity
function _callCallback(contract ISuperApp app, bool isStaticall, bool isTermination, bytes callData, bytes ctx) private returns (bool success, bytes returnedData)
```

### _replacePlaceholderCtx

```solidity
function _replacePlaceholderCtx(bytes data, bytes ctx) internal pure returns (bytes dataWithCtx)
```

_Replace the placeholder ctx with the actual ctx_

### requireValidCtx

```solidity
modifier requireValidCtx(bytes ctx)
```

### assertValidCtx

```solidity
modifier assertValidCtx(bytes ctx)
```

### cleanCtx

```solidity
modifier cleanCtx()
```

### isAgreement

```solidity
modifier isAgreement(contract ISuperAgreement agreementClass)
```

### onlyGovernance

```solidity
modifier onlyGovernance()
```

### onlyAgreement

```solidity
modifier onlyAgreement()
```

### isAppActive

```solidity
modifier isAppActive(contract ISuperApp app)
```

### isValidAppAction

```solidity
modifier isValidAppAction(bytes callData)
```

