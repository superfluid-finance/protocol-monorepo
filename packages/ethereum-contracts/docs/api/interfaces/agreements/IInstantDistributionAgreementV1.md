# Solidity API

## IInstantDistributionAgreementV1

### agreementType

```solidity
function agreementType() external pure returns (bytes32)
```

_ISuperAgreement.agreementType implementation_

### createIndex

```solidity
function createIndex(contract ISuperfluidToken token, uint32 indexId, bytes ctx) external virtual returns (bytes newCtx)
```

_Create a new index for the publisher_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| indexId | uint32 | Id of the index |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks None |

### IndexCreated

```solidity
event IndexCreated(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes userData)
```

_Index created event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index creator and publisher |
| indexId | uint32 | The specified indexId of the newly created index |
| userData | bytes | The user provided data |

### getIndex

```solidity
function getIndex(contract ISuperfluidToken token, address publisher, uint32 indexId) external view virtual returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

_Query the data of a index_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |

| Name | Type | Description |
| ---- | ---- | ----------- |
| exist | bool | Does the index exist |
| indexValue | uint128 | Value of the current index |
| totalUnitsApproved | uint128 | Total units approved for the index |
| totalUnitsPending | uint128 | Total units pending approval for the index |

### calculateDistribution

```solidity
function calculateDistribution(contract ISuperfluidToken token, address publisher, uint32 indexId, uint256 amount) external view virtual returns (uint256 actualAmount, uint128 newIndexValue)
```

_Calculate actual distribution amount_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| amount | uint256 | The amount of tokens desired to be distributed |

| Name | Type | Description |
| ---- | ---- | ----------- |
| actualAmount | uint256 | The amount to be distributed after ensuring no rounding errors |
| newIndexValue | uint128 | The index value given the desired amount of tokens to be distributed |

### updateIndex

```solidity
function updateIndex(contract ISuperfluidToken token, uint32 indexId, uint128 indexValue, bytes ctx) external virtual returns (bytes newCtx)
```

_Update index value of an index_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| indexId | uint32 | Id of the index |
| indexValue | uint128 | Value of the index |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks None |

### IndexUpdated

```solidity
event IndexUpdated(contract ISuperfluidToken token, address publisher, uint32 indexId, uint128 oldIndexValue, uint128 newIndexValue, uint128 totalUnitsPending, uint128 totalUnitsApproved, bytes userData)
```

_Index updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index updater and publisher |
| indexId | uint32 | The specified indexId of the updated index |
| oldIndexValue | uint128 | The previous index value |
| newIndexValue | uint128 | The updated index value |
| totalUnitsPending | uint128 | The total units pending when the indexValue was updated |
| totalUnitsApproved | uint128 | The total units approved when the indexValue was updated |
| userData | bytes | The user provided data |

### distribute

```solidity
function distribute(contract ISuperfluidToken token, uint32 indexId, uint256 amount, bytes ctx) external virtual returns (bytes newCtx)
```

_Distribute tokens through the index_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| indexId | uint32 | Id of the index |
| amount | uint256 | The amount of tokens desired to be distributed |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) NOTE: - This is a convenient version of updateIndex. It adds to the index   a delta that equals to &#x60;amount / totalUnits&#x60; - The actual amount distributed could be obtained via   &#x60;calculateDistribution&#x60;. This is due to precision error with index   value and units data range # App callbacks None |

### approveSubscription

```solidity
function approveSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes ctx) external virtual returns (bytes newCtx)
```

_Approve the subscription of an index_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - if subscription exist   - AgreementCreated callback to the publisher:      - agreementId is for the subscription - if subscription does not exist   - AgreementUpdated callback to the publisher:      - agreementId is for the subscription |

### IndexSubscribed

```solidity
event IndexSubscribed(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes userData)
```

_Index subscribed event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| subscriber | address | The approved subscriber |
| userData | bytes | The user provided data |

### SubscriptionApproved

```solidity
event SubscriptionApproved(contract ISuperfluidToken token, address subscriber, address publisher, uint32 indexId, bytes userData)
```

_Subscription approved event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| subscriber | address | The approved subscriber |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| userData | bytes | The user provided data |

### revokeSubscription

```solidity
function revokeSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes ctx) external virtual returns (bytes newCtx)
```

Revoke the subscription of an index

_&quot;Unapproves&quot; the subscription and moves approved units to pending_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - AgreementUpdated callback to the publisher:    - agreementId is for the subscription |

### IndexUnsubscribed

```solidity
event IndexUnsubscribed(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes userData)
```

_Index unsubscribed event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| subscriber | address | The unsubscribed subscriber |
| userData | bytes | The user provided data |

### SubscriptionRevoked

```solidity
event SubscriptionRevoked(contract ISuperfluidToken token, address subscriber, address publisher, uint32 indexId, bytes userData)
```

_Subscription approved event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| subscriber | address | The approved subscriber |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| userData | bytes | The user provided data |

### updateSubscription

```solidity
function updateSubscription(contract ISuperfluidToken token, uint32 indexId, address subscriber, uint128 units, bytes ctx) external virtual returns (bytes newCtx)
```

_Update the nuber of units of a subscription_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| indexId | uint32 | Id of the index |
| subscriber | address | The subscriber of the index |
| units | uint128 | Number of units of the subscription |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - if subscription exist   - AgreementCreated callback to the subscriber:      - agreementId is for the subscription - if subscription does not exist   - AgreementUpdated callback to the subscriber:      - agreementId is for the subscription |

### IndexUnitsUpdated

```solidity
event IndexUnitsUpdated(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, uint128 units, bytes userData)
```

_Index units updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| subscriber | address | The subscriber units updated |
| units | uint128 | The new units amount |
| userData | bytes | The user provided data |

### SubscriptionUnitsUpdated

```solidity
event SubscriptionUnitsUpdated(contract ISuperfluidToken token, address subscriber, address publisher, uint32 indexId, uint128 units, bytes userData)
```

_Subscription units updated event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| subscriber | address | The subscriber units updated |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| units | uint128 | The new units amount |
| userData | bytes | The user provided data |

### getSubscription

```solidity
function getSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber) external view virtual returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

_Get data of a subscription_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| subscriber | address | The subscriber of the index |

| Name | Type | Description |
| ---- | ---- | ----------- |
| exist | bool | Does the subscription exist? |
| approved | bool | Is the subscription approved? |
| units | uint128 | Units of the suscription |
| pendingDistribution | uint256 | Pending amount of tokens to be distributed for unapproved subscription |

### getSubscriptionByID

```solidity
function getSubscriptionByID(contract ISuperfluidToken token, bytes32 agreementId) external view virtual returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

Get data of a subscription by agreement ID

_indexId (agreementId) is the keccak256 hash of encodePacked(&quot;publisher&quot;, publisher, indexId)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| agreementId | bytes32 | The agreement ID |

| Name | Type | Description |
| ---- | ---- | ----------- |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| approved | bool | Is the subscription approved? |
| units | uint128 | Units of the suscription |
| pendingDistribution | uint256 | Pending amount of tokens to be distributed for unapproved subscription |

### listSubscriptions

```solidity
function listSubscriptions(contract ISuperfluidToken token, address subscriber) external view virtual returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

_List subscriptions of an user_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| subscriber | address | The subscriber&#x27;s address |

| Name | Type | Description |
| ---- | ---- | ----------- |
| publishers | address[] | Publishers of the subcriptions |
| indexIds | uint32[] | Indexes of the subscriptions |
| unitsList | uint128[] | Units of the subscriptions |

### deleteSubscription

```solidity
function deleteSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes ctx) external virtual returns (bytes newCtx)
```

_Delete the subscription of an user_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| subscriber | address | The subscriber&#x27;s address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) # App callbacks - if the subscriber called it   - AgreementTerminated callback to the publsiher:      - agreementId is for the subscription - if the publisher called it   - AgreementTerminated callback to the subscriber:      - agreementId is for the subscription |

### claim

```solidity
function claim(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes ctx) external virtual returns (bytes newCtx)
```

_Claim pending distributions_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| subscriber | address | The subscriber&#x27;s address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) The subscription should not be approved yet # App callbacks - AgreementUpdated callback to the publisher:    - agreementId is for the subscription |

### IndexDistributionClaimed

```solidity
event IndexDistributionClaimed(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, uint256 amount)
```

_Index distribution claimed event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| subscriber | address | The subscriber units updated |
| amount | uint256 | The pending amount claimed |

### SubscriptionDistributionClaimed

```solidity
event SubscriptionDistributionClaimed(contract ISuperfluidToken token, address subscriber, address publisher, uint32 indexId, uint256 amount)
```

_Subscription distribution claimed event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| subscriber | address | The subscriber units updated |
| publisher | address | Index publisher |
| indexId | uint32 | The specified indexId |
| amount | uint256 | The pending amount claimed |

