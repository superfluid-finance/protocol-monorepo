# IInstantDistributionAgreementV1

## Functions

### agreementType

```solidity
function agreementType(
) external returns (bytes32)
```

ISuperAgreement.agreementType implementation

### createIndex

```solidity
function createIndex(
    contract ISuperfluidToken token,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

Create a new index for the publisher

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `indexId` | uint32 | Id of the index |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks

None |

### getIndex

```solidity
function getIndex(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) external returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

Query the data of a index

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exist` | bool | Does the index exist |
| `indexValue` | uint128 | Value of the current index |
| `totalUnitsApproved` | uint128 | Total units approved for the index |
| `totalUnitsPending` | uint128 | Total units pending approval for the index |

### calculateDistribution

```solidity
function calculateDistribution(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    uint256 amount
) external returns (uint256 actualAmount, uint128 newIndexValue)
```

Calculate actual distribution amount

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `amount` | uint256 | The amount of tokens desired to be distributed |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `actualAmount` | uint256 | The amount to be distributed after ensuring no rounding errors |
| `newIndexValue` | uint128 | The index value given the desired amount of tokens to be distributed |

### updateIndex

```solidity
function updateIndex(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue,
    bytes ctx
) external returns (bytes newCtx)
```

Update index value of an index

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `indexId` | uint32 | Id of the index |
| `indexValue` | uint128 | Value of the index |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks

None |

### distribute

```solidity
function distribute(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount,
    bytes ctx
) external returns (bytes newCtx)
```

Distribute tokens through the index

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `indexId` | uint32 | Id of the index |
| `amount` | uint256 | The amount of tokens desired to be distributed |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

NOTE:
- This is a convenient version of updateIndex. It adds to the index
  a delta that equals to `amount / totalUnits`
- The actual amount distributed could be obtained via
  `calculateDistribution`. This is due to precision error with index
  value and units data range

# App callbacks

None |

### approveSubscription

```solidity
function approveSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

Approve the subscription of an index

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks

- if subscription exist
  - AgreementCreated callback to the publisher:
     - agreementId is for the subscription
- if subscription does not exist
  - AgreementUpdated callback to the publisher:
     - agreementId is for the subscription |

### revokeSubscription

```solidity
function revokeSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes ctx
) external returns (bytes newCtx)
```

Revoke the subscription of an index

"Unapproves" the subscription and moves approved units to pending

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks
- AgreementUpdated callback to the publisher:
   - agreementId is for the subscription |

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

Update the nuber of units of a subscription

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `indexId` | uint32 | Id of the index |
| `subscriber` | address | The subscriber of the index |
| `units` | uint128 | Number of units of the subscription |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks

- if subscription exist
  - AgreementCreated callback to the subscriber:
     - agreementId is for the subscription
- if subscription does not exist
  - AgreementUpdated callback to the subscriber:
     - agreementId is for the subscription |

### getSubscription

```solidity
function getSubscription(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) external returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

Get data of a subscription

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `subscriber` | address | The subscriber of the index |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exist` | bool | Does the subscription exist? |
| `approved` | bool | Is the subscription approved? |
| `units` | uint128 | Units of the suscription |
| `pendingDistribution` | uint256 | Pending amount of tokens to be distributed for unapproved subscription |

### getSubscriptionByID

```solidity
function getSubscriptionByID(
    contract ISuperfluidToken token,
    bytes32 agreementId
) external returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

Get data of a subscription by agreement ID

indexId (agreementId) is the keccak256 hash of encodePacked("publisher", publisher, indexId)

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `agreementId` | bytes32 | The agreement ID |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `approved` | bool | Is the subscription approved? |
| `units` | uint128 | Units of the suscription |
| `pendingDistribution` | uint256 | Pending amount of tokens to be distributed for unapproved subscription |

### listSubscriptions

```solidity
function listSubscriptions(
    contract ISuperfluidToken token,
    address subscriber
) external returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

List subscriptions of an user

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `subscriber` | address | The subscriber's address |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publishers` | address[] | Publishers of the subcriptions |
| `indexIds` | uint32[] | Indexes of the subscriptions |
| `unitsList` | uint128[] | Units of the subscriptions |

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

Delete the subscription of an user

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | The publisher of the index |
| `indexId` | uint32 | Id of the index |
| `subscriber` | address | The subscriber's address |
| `ctx` | bytes | Context bytes (see ISuperfluid.sol for Context struct)

# App callbacks

- if the subscriber called it
  - AgreementTerminated callback to the publsiher:
     - agreementId is for the subscription
- if the publisher called it
  - AgreementTerminated callback to the subscriber:
     - agreementId is for the subscription |

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

## Events

### IndexCreated

```solidity
event IndexCreated(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
)
```

Index created event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index creator and publisher |
| `indexId` | uint32 | The specified indexId of the newly created index |
| `userData` | bytes | The user provided data |
### IndexUpdated

```solidity
event IndexUpdated(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    uint128 oldIndexValue,
    uint128 newIndexValue,
    uint128 totalUnitsPending,
    uint128 totalUnitsApproved,
    bytes userData
)
```

Index updated event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index updater and publisher |
| `indexId` | uint32 | The specified indexId of the updated index |
| `oldIndexValue` | uint128 | The previous index value |
| `newIndexValue` | uint128 | The updated index value |
| `totalUnitsPending` | uint128 | The total units pending when the indexValue was updated |
| `totalUnitsApproved` | uint128 | The total units approved when the indexValue was updated |
| `userData` | bytes | The user provided data |
### IndexSubscribed

```solidity
event IndexSubscribed(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
)
```

Index subscribed event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `subscriber` | address | The approved subscriber |
| `userData` | bytes | The user provided data |
### SubscriptionApproved

```solidity
event SubscriptionApproved(
    contract ISuperfluidToken token,
    address subscriber,
    address publisher,
    uint32 indexId,
    bytes userData
)
```

Subscription approved event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `subscriber` | address | The approved subscriber |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `userData` | bytes | The user provided data |
### IndexUnsubscribed

```solidity
event IndexUnsubscribed(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
)
```

Index unsubscribed event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `subscriber` | address | The unsubscribed subscriber |
| `userData` | bytes | The user provided data |
### SubscriptionRevoked

```solidity
event SubscriptionRevoked(
    contract ISuperfluidToken token,
    address subscriber,
    address publisher,
    uint32 indexId,
    bytes userData
)
```

Subscription approved event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `subscriber` | address | The approved subscriber |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `userData` | bytes | The user provided data |
### IndexUnitsUpdated

```solidity
event IndexUnitsUpdated(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    uint128 units,
    bytes userData
)
```

Index units updated event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `subscriber` | address | The subscriber units updated |
| `units` | uint128 | The new units amount |
| `userData` | bytes | The user provided data |
### SubscriptionUnitsUpdated

```solidity
event SubscriptionUnitsUpdated(
    contract ISuperfluidToken token,
    address subscriber,
    address publisher,
    uint32 indexId,
    uint128 units,
    bytes userData
)
```

Subscription units updated event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `subscriber` | address | The subscriber units updated |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `units` | uint128 | The new units amount |
| `userData` | bytes | The user provided data |
### IndexDistributionClaimed

```solidity
event IndexDistributionClaimed(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    uint256 amount
)
```

Index distribution claimed event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `subscriber` | address | The subscriber units updated |
| `amount` | uint256 | The pending amount claimed |
### SubscriptionDistributionClaimed

```solidity
event SubscriptionDistributionClaimed(
    contract ISuperfluidToken token,
    address subscriber,
    address publisher,
    uint32 indexId,
    uint256 amount
)
```

Subscription distribution claimed event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken | Super token address |
| `subscriber` | address | The subscriber units updated |
| `publisher` | address | Index publisher |
| `indexId` | uint32 | The specified indexId |
| `amount` | uint256 | The pending amount claimed |

