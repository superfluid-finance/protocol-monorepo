# InstantDistributionAgreementV1

Please read IInstantDistributionAgreementV1 for implementation notes.
For more technical notes, please visit protocol-monorepo wiki area.

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

### realtimeBalanceOf

```solidity
function realtimeBalanceOf(
    contract ISuperfluidToken token,
    address account,
    uint256 
) external returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
```

ISuperAgreement.realtimeBalanceOf implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `account` | address |  |
| `` | uint256 |  |

### createIndex

```solidity
function createIndex(
    contract ISuperfluidToken token,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.createIndex implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `ctx` | bytes |  |

### getIndex

```solidity
function getIndex(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) external returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

IInstantDistributionAgreementV1.getIndex implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### calculateDistribution

```solidity
function calculateDistribution(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    uint256 amount
) external returns (uint256 actualAmount, uint128 newIndexValue)
```

IInstantDistributionAgreementV1.calculateDistribution implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `amount` | uint256 |  |

### updateIndex

```solidity
function updateIndex(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.updateIndex implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `indexValue` | uint128 |  |
| `ctx` | bytes |  |

### distribute

```solidity
function distribute(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.distribute implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `amount` | uint256 |  |
| `ctx` | bytes |  |

### _updateIndex

```solidity
function _updateIndex(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes32 iId,
    struct InstantDistributionAgreementV1.IndexData idata,
    uint128 newIndexValue,
    bytes userData
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `iId` | bytes32 |  |
| `idata` | struct InstantDistributionAgreementV1.IndexData |  |
| `newIndexValue` | uint128 |  |
| `userData` | bytes |  |

### _loadIndexData

```solidity
function _loadIndexData(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) private returns (bytes32 iId, struct InstantDistributionAgreementV1.IndexData idata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### approveSubscription

```solidity
function approveSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.approveSubscription implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `ctx` | bytes |  |

### revokeSubscription

```solidity
function revokeSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.revokeSubscription implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `ctx` | bytes |  |

### updateSubscription

```solidity
function updateSubscription(
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.updateSubscription implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `units` | uint128 |  |
| `ctx` | bytes |  |

### getSubscription

```solidity
function getSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) external returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

IInstantDistributionAgreementV1.getSubscription implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |

### getSubscriptionByID

```solidity
function getSubscriptionByID(
    contract ISuperfluidToken token,
    bytes32 agreementId
) external returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

IInstantDistributionAgreementV1.getSubscriptionByID implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `agreementId` | bytes32 |  |

### listSubscriptions

```solidity
function listSubscriptions(
    contract ISuperfluidToken token,
    address subscriber
) external returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

IInstantDistributionAgreementV1.listSubscriptions implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `subscriber` | address |  |

### deleteSubscription

```solidity
function deleteSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes ctx
) external returns (bytes newCtx)
```

IInstantDistributionAgreementV1.deleteSubscription implementation

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `ctx` | bytes |  |

### claim

```solidity
function claim(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes ctx
) external returns (bytes newCtx)
```

Claim pending distributions

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `subscriber` | address | The subscriber's address |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

The subscription should not be approved yet

# App callbacks

- AgreementUpdated callback to the publisher:
   - agreementId is for the subscription |

### _loadAllData

```solidity
function _loadAllData(
    contract ISuperfluidToken token,
    address publisher,
    address subscriber,
    uint32 indexId,
    bool requireSubscriptionExisting
) private returns (bytes32 iId, bytes32 sId, struct InstantDistributionAgreementV1.IndexData idata, bool subscriptionExists, struct InstantDistributionAgreementV1.SubscriptionData sdata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `subscriber` | address |  |
| `indexId` | uint32 |  |
| `requireSubscriptionExisting` | bool |  |

### _getPublisherId

```solidity
function _getPublisherId(
    address publisher,
    uint32 indexId
) private returns (bytes32 iId)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### _getSubscriptionId

```solidity
function _getSubscriptionId(
    address subscriber,
    bytes32 iId
) private returns (bytes32 sId)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `subscriber` | address |  |
| `iId` | bytes32 |  |

### _encodeIndexData

```solidity
function _encodeIndexData(
    struct InstantDistributionAgreementV1.IndexData idata
) private returns (bytes32[] data)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idata` | struct InstantDistributionAgreementV1.IndexData |  |

### _hasIndexData

```solidity
function _hasIndexData(
    contract ISuperfluidToken token,
    bytes32 iId
) private returns (bool exist)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `iId` | bytes32 |  |

### _getIndexData

```solidity
function _getIndexData(
    contract ISuperfluidToken token,
    bytes32 iId
) private returns (bool exist, struct InstantDistributionAgreementV1.IndexData idata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `iId` | bytes32 |  |

### _getPublisherDeposit

```solidity
function _getPublisherDeposit(
    contract ISuperfluidToken token,
    address publisher
) private returns (uint256)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |

### _adjustPublisherDeposit

```solidity
function _adjustPublisherDeposit(
    contract ISuperfluidToken token,
    address publisher,
    int256 delta
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `delta` | int256 |  |

### _encodeSubscriptionData

```solidity
function _encodeSubscriptionData(
    struct InstantDistributionAgreementV1.SubscriptionData sdata
) private returns (bytes32[] data)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `sdata` | struct InstantDistributionAgreementV1.SubscriptionData |  |

### _getSubscriptionData

```solidity
function _getSubscriptionData(
    contract ISuperfluidToken token,
    bytes32 sId
) private returns (bool exist, struct InstantDistributionAgreementV1.SubscriptionData sdata)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `sId` | bytes32 |  |

### _findAndFillSubsBitmap

```solidity
function _findAndFillSubsBitmap(
    contract ISuperfluidToken token,
    address subscriber,
    bytes32 iId
) private returns (uint32 subId)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `subscriber` | address |  |
| `iId` | bytes32 |  |

### _clearSubsBitmap

```solidity
function _clearSubsBitmap(
    contract ISuperfluidToken token,
    address subscriber,
    uint32 subId
) private
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `subscriber` | address |  |
| `subId` | uint32 |  |

### _listSubscriptionIds

```solidity
function _listSubscriptionIds(
    contract ISuperfluidToken token,
    address subscriber
) private returns (uint32[] slotIds, bytes32[] sidList)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `subscriber` | address |  |

