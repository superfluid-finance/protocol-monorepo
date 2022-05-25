# IDAv1Library

Set a variable of type `InitData` in the contract, then call this library's functions
directly `initData.functionName()`.

## Functions

### getIndex

```solidity
function getIndex(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) internal returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

Gets an index by its ID and publisher.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exist` | bool | True if the index exists. |
| `indexValue` | uint128 | Total value of the index. |
| `totalUnitsApproved` | uint128 | Units of the index approved by subscribers. |
| `totalUnitsPending` | uint128 | Units of teh index not yet approved by subscribers. |

### calculateDistribution

```solidity
function calculateDistribution(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    uint256 amount
) internal returns (uint256 actualAmount, uint128 newIndexValue)
```

Calculates the distribution amount based on the amount of tokens desired to distribute.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `amount` | uint256 | Amount of tokens desired to distribute. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `actualAmount` | uint256 | Amount to be distributed with correct rounding. |
| `newIndexValue` | uint128 | The index value after the distribution would be called. |

### listSubscriptions

```solidity
function listSubscriptions(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address subscriber
) internal returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

List all subscriptions of an address

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super token used in the indexes listed. |
| `subscriber` | address | Subscriber address. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publishers` | address[] | Publishers of the indices. |
| `indexIds` | uint32[] | IDs of the indices. |
| `unitsList` | uint128[] | Units owned of the indices. |

### getSubscription

```solidity
function getSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) internal returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

Gets subscription by publisher, index id, and subscriber.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber to the index. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `exist` | bool | True if the subscription exists. |
| `approved` | bool | True if the subscription has been approved by the subscriber. |
| `units` | uint128 | Units held by the subscriber |
| `pendingDistribution` | uint256 | If not approved, the amount to be claimed on approval. |

### getSubscriptionByID

```solidity
function getSubscriptionByID(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    bytes32 agreementId
) internal returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

Gets subscription by the agreement ID.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `agreementId` | bytes32 | Agreement ID, unique to the subscriber and index ID. |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `approved` | bool | True if the subscription has been approved by the subscriber. |
| `units` | uint128 | Units held by the subscriber |
| `pendingDistribution` | uint256 | If not approved, the amount to be claimed on approval. |

### createIndex

```solidity
function createIndex(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId
) internal
```

Creates a new index.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |

### createIndex

```solidity
function createIndex(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    bytes userData
) internal
```

Creates a new index. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### createIndexWithCtx

```solidity
function createIndexWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId
) internal returns (bytes newCtx)
```

Creates a new index in a super app callback.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |

### createIndexWithCtx

```solidity
function createIndexWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    bytes userData
) internal returns (bytes newCtx)
```

Creates a new index in a super app callback. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### updateIndexValue

```solidity
function updateIndexValue(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue
) internal
```

Updates an index value. This distributes an amount of tokens equal to
`indexValue - lastIndexValue`. See `distribute` for another way to distribute. This takes
arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `indexValue` | uint128 | New TOTAL index value, this will equal the total amount distributed. |

### updateIndexValue

```solidity
function updateIndexValue(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue,
    bytes userData
) internal
```

Updates an index value. This distributes an amount of tokens equal to
`indexValue - lastIndexValue`. See `distribute` for another way to distribute.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `indexValue` | uint128 | New TOTAL index value, this will equal the total amount distributed. |
| `userData` | bytes | Arbitrary user data field. |

### updateIndexValueWithCtx

```solidity
function updateIndexValueWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue
) internal returns (bytes newCtx)
```

Updates an index value in a super app callback. This distributes an amount of tokens
equal to `indexValue - lastIndexValue`. See `distribute` for another way to distribute.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `indexValue` | uint128 | New TOTAL index value, this will equal the total amount distributed. |

### updateIndexValueWithCtx

```solidity
function updateIndexValueWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue,
    bytes userData
) internal returns (bytes newCtx)
```

Updates an index value in a super app callback. This distributes an amount of tokens
equal to `indexValue - lastIndexValue`. See `distribute` for another way to distribute.
This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `indexValue` | uint128 | New TOTAL index value, this will equal the total amount distributed. |
| `userData` | bytes |  |

### distribute

```solidity
function distribute(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount
) internal
```

Distributes tokens in a more developer friendly way than `updateIndex`. Instead of
passing the new total index value, this function will increase the index value by `amount`.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `amount` | uint256 | Amount by which the index value should increase. |

### distribute

```solidity
function distribute(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount,
    bytes userData
) internal
```

Distributes tokens in a more developer friendly way than `updateIndex`. Instead of
passing the new total index value, this function will increase the index value by `amount`.
This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `amount` | uint256 | Amount by which the index value should increase. |
| `userData` | bytes | Arbitrary user data field. |

### distributeWithCtx

```solidity
function distributeWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount
) internal returns (bytes newCtx)
```

Distributes tokens in a super app callback. Instead of passing the new total index
value, this function will increase the index value by `amount`.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `amount` | uint256 | Amount by which the index value should increase. |

### distributeWithCtx

```solidity
function distributeWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount,
    bytes userData
) internal returns (bytes newCtx)
```

Distributes tokens in a super app callback. Instead of passing the new total index
value, this function will increase the index value by `amount`. This takes arbitrary user
data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `amount` | uint256 | Amount by which the index value should increase. |
| `userData` | bytes | Arbitrary user data field. |

### approveSubscription

```solidity
function approveSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) internal
```

Approves a subscription to an index. The subscriber's real time balance will not update
until the subscription is approved, but once approved, the balance will be updated with
prior distributions.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |

### approveSubscription

```solidity
function approveSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) internal
```

Approves a subscription to an index. The subscriber's real time balance will not update
until the subscription is approved, but once approved, the balance will be updated with
prior distributions.
This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### approveSubscriptionWithCtx

```solidity
function approveSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) internal returns (bytes newCtx)
```

Approves a subscription to an index in a super app callback. The subscriber's real time
balance will not update until the subscription is approved, but once approved, the balance
will be updated with prior distributions.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |

### approveSubscriptionWithCtx

```solidity
function approveSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) internal returns (bytes newCtx)
```

Approves a subscription to an index in a super app callback. The subscriber's real time
balance will not update until the subscription is approved, but once approved, the balance
will be updated with prior distributions. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### revokeSubscription

```solidity
function revokeSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) internal
```

Revokes a previously approved subscription.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |

### revokeSubscription

```solidity
function revokeSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) internal
```

Revokes a previously approved subscription. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### revokeSubscriptionWithCtx

```solidity
function revokeSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) internal returns (bytes newCtx)
```

Revokes a previously approved subscription in a super app callback.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |

### revokeSubscriptionWithCtx

```solidity
function revokeSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) internal returns (bytes newCtx)
```

Revokes a previously approved subscription in a super app callback. This takes
arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `userData` | bytes | Arbitrary user data field. |

### updateSubscriptionUnits

```solidity
function updateSubscriptionUnits(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units
) internal
```

Updates the units of a subscription. This changes the number of shares the subscriber
holds.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be updated. |
| `units` | uint128 | New number of units the subscriber holds. |

### updateSubscriptionUnits

```solidity
function updateSubscriptionUnits(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units,
    bytes userData
) internal
```

Updates the units of a subscription. This changes the number of shares the subscriber
holds. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be updated. |
| `units` | uint128 | New number of units the subscriber holds. |
| `userData` | bytes | Arbitrary user data field. |

### updateSubscriptionUnitsWithCtx

```solidity
function updateSubscriptionUnitsWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units
) internal returns (bytes newCtx)
```

Updates the units of a subscription in a super app callback. This changes the number of
shares the subscriber holds.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be updated. |
| `units` | uint128 | New number of units the subscriber holds. |

### updateSubscriptionUnitsWithCtx

```solidity
function updateSubscriptionUnitsWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units,
    bytes userData
) internal returns (bytes newCtx)
```

Updates the units of a subscription in a super app callback. This changes the number of
shares the subscriber holds. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be updated. |
| `units` | uint128 | New number of units the subscriber holds. |
| `userData` | bytes | Arbitrary user data field. |

### deleteSubscription

```solidity
function deleteSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) internal
```

Deletes a subscription, setting a subcriber's units to zero.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be deleted. |

### deleteSubscription

```solidity
function deleteSubscription(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) internal
```

Deletes a subscription, setting a subcriber's units to zero. This takes arbitrary user
data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be deleted. |
| `userData` | bytes | Arbitrary user data field. |

### deleteSubscriptionWithCtx

```solidity
function deleteSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) internal returns (bytes newCtx)
```

Deletes a subscription in a super app callback, setting a subcriber's units to zero.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be deleted. |

### deleteSubscriptionWithCtx

```solidity
function deleteSubscriptionWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) internal returns (bytes newCtx)
```

Deletes a subscription in a super app callback, setting a subcriber's units to zero.
This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes | Context byte string used by the Superfluid host. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address whose units are to be deleted. |
| `userData` | bytes | Arbitrary user data field. |

### claim

```solidity
function claim(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) internal
```

Claims pending distribution. Subscription should not be approved.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address that receives the claim. |

### claim

```solidity
function claim(
    struct IDAv1Library.InitData idaLibrary,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) internal
```

Claims pending distribution. Subscription should not be approved. This takes arbitrary
user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address that receives the claim. |
| `userData` | bytes | Arbitrary user data field. |

### claimWithCtx

```solidity
function claimWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) internal returns (bytes newCtx)
```

Claims pending distribution in a super app callback. Subscription should not be
approved.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes |  |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address that receives the claim. |

### claimWithCtx

```solidity
function claimWithCtx(
    struct IDAv1Library.InitData idaLibrary,
    bytes ctx,
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) internal returns (bytes newCtx)
```

Claims pending distribution in a super app callback. Subscription should not be
approved. This takes arbitrary user data.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `idaLibrary` | struct IDAv1Library.InitData | Storage pointer to host and ida interfaces. |
| `ctx` | bytes |  |
| `token` | contract ISuperfluidToken | Super Token used with the index. |
| `publisher` | address | Publisher of the index. |
| `indexId` | uint32 | ID of the index. |
| `subscriber` | address | Subscriber address that receives the claim. |
| `userData` | bytes | Arbitrary user data field. |

