# Solidity API

## ISuperfluid

### getNow

```solidity
function getNow() external view returns (uint256)
```

### getGovernance

```solidity
function getGovernance() external view returns (contract ISuperfluidGovernance governance)
```

_Get the current governance address of the Superfluid host_

### replaceGovernance

```solidity
function replaceGovernance(contract ISuperfluidGovernance newGov) external
```

_Replace the current governance with a new one_

### GovernanceReplaced

```solidity
event GovernanceReplaced(contract ISuperfluidGovernance oldGov, contract ISuperfluidGovernance newGov)
```

_Governance replaced event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| oldGov | contract ISuperfluidGovernance | Address of the old governance contract |
| newGov | contract ISuperfluidGovernance | Address of the new governance contract |

### registerAgreementClass

```solidity
function registerAgreementClass(contract ISuperAgreement agreementClassLogic) external
```

_Register a new agreement class to the system_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementClassLogic | contract ISuperAgreement | Initial agreement class code Modifiers:  - onlyGovernance |

### AgreementClassRegistered

```solidity
event AgreementClassRegistered(bytes32 agreementType, address code)
```

Agreement class registered event

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementType | bytes32 | The agreement type registered |
| code | address | Address of the new agreement |

### updateAgreementClass

```solidity
function updateAgreementClass(contract ISuperAgreement agreementClassLogic) external
```

_Update code of an agreement class_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementClassLogic | contract ISuperAgreement | New code for the agreement class Modifiers:  - onlyGovernance |

### AgreementClassUpdated

```solidity
event AgreementClassUpdated(bytes32 agreementType, address code)
```

Agreement class updated event

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

| Name | Type | Description |
| ---- | ---- | ----------- |
| agreementType | bytes32 | The agreement type updated |
| code | address | Address of the new agreement |

### isAgreementTypeListed

```solidity
function isAgreementTypeListed(bytes32 agreementType) external view returns (bool yes)
```

Check if the agreement type is whitelisted

_agreementType is the keccak256 hash of: &quot;org.superfluid-finance.agreements.&lt;AGREEMENT_NAME&gt;.&lt;VERSION&gt;&quot;_

### isAgreementClassListed

```solidity
function isAgreementClassListed(contract ISuperAgreement agreementClass) external view returns (bool yes)
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

### SuperTokenFactoryUpdated

```solidity
event SuperTokenFactoryUpdated(contract ISuperTokenFactory newFactory)
```

_SuperToken factory updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| newFactory | contract ISuperTokenFactory | Address of the new factory |

### updateSuperTokenLogic

```solidity
function updateSuperTokenLogic(contract ISuperToken token) external
```

Update the super token logic to the latest

_Refer to ISuperTokenFactory.Upgradability for expected behaviours_

### SuperTokenLogicUpdated

```solidity
event SuperTokenLogicUpdated(contract ISuperToken token, address code)
```

_SuperToken logic updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken |  |
| code | address | Address of the new SuperToken logic |

### registerApp

```solidity
function registerApp(uint256 configWord) external
```

_Message sender (must be a contract) declares itself as a super app._

| Name | Type | Description |
| ---- | ---- | ----------- |
| configWord | uint256 | The super app manifest configuration, flags are defined in &#x60;SuperAppDefinitions&#x60; |

### AppRegistered

```solidity
event AppRegistered(contract ISuperApp app)
```

_App registered event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Address of jailed app |

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

### isApp

```solidity
function isApp(contract ISuperApp app) external view returns (bool)
```

_Query if the app is registered_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |

### getAppLevel

```solidity
function getAppLevel(contract ISuperApp app) external view returns (uint8 appLevel)
```

_Query app level_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Super app address |

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
function isAppJailed(contract ISuperApp app) external view returns (bool isJail)
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
function isCompositeAppAllowed(contract ISuperApp app, contract ISuperApp targetApp) external view returns (bool isAppAllowed)
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
function appCallbackPush(bytes ctx, contract ISuperApp app, uint256 appAllowanceGranted, int256 appAllowanceUsed, contract ISuperfluidToken appAllowanceToken) external returns (bytes newCtx)
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
| newCtx | bytes | The current context of the transaction. |

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

### Jail

```solidity
event Jail(contract ISuperApp app, uint256 reason)
```

_Jail event for the app_

| Name | Type | Description |
| ---- | ---- | ----------- |
| app | contract ISuperApp | Address of jailed app |
| reason | uint256 | Reason the app is jailed (see Definitions.sol for the full list) |

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

### Context

```solidity
struct Context {
  uint8 appLevel;
  uint8 callType;
  uint256 timestamp;
  address msgSender;
  bytes4 agreementSelector;
  bytes userData;
  uint256 appAllowanceGranted;
  uint256 appAllowanceWanted;
  int256 appAllowanceUsed;
  address appAddress;
  contract ISuperfluidToken appAllowanceToken;
}
```

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
function decodeCtx(bytes ctx) external pure returns (struct ISuperfluid.Context context)
```

### isCtxValid

```solidity
function isCtxValid(bytes ctx) external view returns (bool)
```

### Operation

```solidity
struct Operation {
  uint32 operationType;
  address target;
  bytes data;
}
```

### batchCall

```solidity
function batchCall(struct ISuperfluid.Operation[] operations) external
```

_Batch call function_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operations | struct ISuperfluid.Operation[] | Array of batch operations |

### forwardBatchCall

```solidity
function forwardBatchCall(struct ISuperfluid.Operation[] operations) external
```

_Batch call function for trusted forwarders (EIP-2771)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| operations | struct ISuperfluid.Operation[] | Array of batch operations |

