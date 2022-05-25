# Solidity API

## InstantDistributionAgreementV1

_Please read IInstantDistributionAgreementV1 for implementation notes.
For more technical notes, please visit protocol-monorepo wiki area._

### SLOTS_BITMAP_LIBRARY_ADDRESS

```solidity
address SLOTS_BITMAP_LIBRARY_ADDRESS
```

### _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID

```solidity
uint256 _SUBSCRIBER_SUBS_BITMAP_STATE_SLOT_ID
```

_Subscriber state slot id for storing subs bitmap_

### _PUBLISHER_DEPOSIT_STATE_SLOT_ID

```solidity
uint256 _PUBLISHER_DEPOSIT_STATE_SLOT_ID
```

_Publisher state slot id for storing its deposit amount_

### _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START

```solidity
uint256 _SUBSCRIBER_SUB_DATA_STATE_SLOT_ID_START
```

_Subscriber state slot id starting ptoint for subscription data_

### _MAX_NUM_SUBS

```solidity
uint32 _MAX_NUM_SUBS
```

_Maximum number of subscriptions a subscriber can have_

### _UNALLOCATED_SUB_ID

```solidity
uint32 _UNALLOCATED_SUB_ID
```

_A special id that indicating the subscription is not approved yet_

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### IndexData

```solidity
struct IndexData {
  uint128 indexValue;
  uint128 totalUnitsApproved;
  uint128 totalUnitsPending;
}
```

### SubscriptionData

```solidity
struct SubscriptionData {
  uint32 subId;
  address publisher;
  uint32 indexId;
  uint128 indexValue;
  uint128 units;
}
```

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(contract ISuperfluidToken token, address account, uint256) external view returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

_ISuperAgreement.realtimeBalanceOf implementation_

### createIndex

```solidity
function createIndex(contract ISuperfluidToken token, uint32 indexId, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.createIndex implementation_

### getIndex

```solidity
function getIndex(contract ISuperfluidToken token, address publisher, uint32 indexId) external view returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

_IInstantDistributionAgreementV1.getIndex implementation_

### calculateDistribution

```solidity
function calculateDistribution(contract ISuperfluidToken token, address publisher, uint32 indexId, uint256 amount) external view returns (uint256 actualAmount, uint128 newIndexValue)
```

_IInstantDistributionAgreementV1.calculateDistribution implementation_

### updateIndex

```solidity
function updateIndex(contract ISuperfluidToken token, uint32 indexId, uint128 indexValue, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.updateIndex implementation_

### distribute

```solidity
function distribute(contract ISuperfluidToken token, uint32 indexId, uint256 amount, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.distribute implementation_

### _updateIndex

```solidity
function _updateIndex(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes32 iId, struct InstantDistributionAgreementV1.IndexData idata, uint128 newIndexValue, bytes userData) private
```

### _loadIndexData

```solidity
function _loadIndexData(contract ISuperfluidToken token, address publisher, uint32 indexId) private view returns (bytes32 iId, struct InstantDistributionAgreementV1.IndexData idata)
```

### _SubscriptionOperationVars

```solidity
struct _SubscriptionOperationVars {
  bytes32 iId;
  bool subscriptionExists;
  bytes32 sId;
  struct InstantDistributionAgreementV1.IndexData idata;
  struct InstantDistributionAgreementV1.SubscriptionData sdata;
  bytes cbdata;
}
```

### approveSubscription

```solidity
function approveSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.approveSubscription implementation_

### revokeSubscription

```solidity
function revokeSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.revokeSubscription implementation_

### updateSubscription

```solidity
function updateSubscription(contract ISuperfluidToken token, uint32 indexId, address subscriber, uint128 units, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.updateSubscription implementation_

### getSubscription

```solidity
function getSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber) external view returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

_IInstantDistributionAgreementV1.getSubscription implementation_

### getSubscriptionByID

```solidity
function getSubscriptionByID(contract ISuperfluidToken token, bytes32 agreementId) external view returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

_IInstantDistributionAgreementV1.getSubscriptionByID implementation_

### listSubscriptions

```solidity
function listSubscriptions(contract ISuperfluidToken token, address subscriber) external view returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

_IInstantDistributionAgreementV1.listSubscriptions implementation_

### deleteSubscription

```solidity
function deleteSubscription(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes ctx) external returns (bytes newCtx)
```

_IInstantDistributionAgreementV1.deleteSubscription implementation_

### claim

```solidity
function claim(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes ctx) external returns (bytes newCtx)
```

_Claim pending distributions_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperfluidToken | Super token address |
| publisher | address | The publisher of the index |
| indexId | uint32 | Id of the index |
| subscriber | address | The subscriber&#x27;s address |
| ctx | bytes | Context bytes (see ISuperfluid.sol for Context struct) The subscription should not be approved yet # App callbacks - AgreementUpdated callback to the publisher:    - agreementId is for the subscription |

### _loadAllData

```solidity
function _loadAllData(contract ISuperfluidToken token, address publisher, address subscriber, uint32 indexId, bool requireSubscriptionExisting) private view returns (bytes32 iId, bytes32 sId, struct InstantDistributionAgreementV1.IndexData idata, bool subscriptionExists, struct InstantDistributionAgreementV1.SubscriptionData sdata)
```

### _getPublisherId

```solidity
function _getPublisherId(address publisher, uint32 indexId) private pure returns (bytes32 iId)
```

### _getSubscriptionId

```solidity
function _getSubscriptionId(address subscriber, bytes32 iId) private pure returns (bytes32 sId)
```

### _encodeIndexData

```solidity
function _encodeIndexData(struct InstantDistributionAgreementV1.IndexData idata) private pure returns (bytes32[] data)
```

### _hasIndexData

```solidity
function _hasIndexData(contract ISuperfluidToken token, bytes32 iId) private view returns (bool exist)
```

### _getIndexData

```solidity
function _getIndexData(contract ISuperfluidToken token, bytes32 iId) private view returns (bool exist, struct InstantDistributionAgreementV1.IndexData idata)
```

### _getPublisherDeposit

```solidity
function _getPublisherDeposit(contract ISuperfluidToken token, address publisher) private view returns (uint256)
```

### _adjustPublisherDeposit

```solidity
function _adjustPublisherDeposit(contract ISuperfluidToken token, address publisher, int256 delta) private
```

### _encodeSubscriptionData

```solidity
function _encodeSubscriptionData(struct InstantDistributionAgreementV1.SubscriptionData sdata) private pure returns (bytes32[] data)
```

### _getSubscriptionData

```solidity
function _getSubscriptionData(contract ISuperfluidToken token, bytes32 sId) private view returns (bool exist, struct InstantDistributionAgreementV1.SubscriptionData sdata)
```

### _findAndFillSubsBitmap

```solidity
function _findAndFillSubsBitmap(contract ISuperfluidToken token, address subscriber, bytes32 iId) private returns (uint32 subId)
```

### _clearSubsBitmap

```solidity
function _clearSubsBitmap(contract ISuperfluidToken token, address subscriber, uint32 subId) private
```

### _listSubscriptionIds

```solidity
function _listSubscriptionIds(contract ISuperfluidToken token, address subscriber) private view returns (uint32[] slotIds, bytes32[] sidList)
```

