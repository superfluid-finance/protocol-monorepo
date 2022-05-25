# IDAv1LibraryMock

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

### getIndexTest

```solidity
function getIndexTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) external returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### calculateDistributionTest

```solidity
function calculateDistributionTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    uint256 amount
) external returns (uint256 actualAmount, uint128 newIndexValue)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `amount` | uint256 |  |

### listSubscriptionsTest

```solidity
function listSubscriptionsTest(
    contract ISuperfluidToken token,
    address subscriber
) external returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `subscriber` | address |  |

### getSubscriptionTest

```solidity
function getSubscriptionTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) external returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |

### getSubscriptionByIDTest

```solidity
function getSubscriptionByIDTest(
    contract ISuperfluidToken token,
    bytes32 agreementId
) external returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

agreementId == keccak256(abi.encodePacked("subscription", subscriber, indexId));

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `agreementId` | bytes32 |  |

### createIndexTest

```solidity
function createIndexTest(
    contract ISuperfluidToken token,
    uint32 indexId
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |

### createIndexWithUserDataTest

```solidity
function createIndexWithUserDataTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `userData` | bytes |  |

### updateIndexValueTest

```solidity
function updateIndexValueTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `indexValue` | uint128 |  |

### updateIndexValueWithUserDataTest

```solidity
function updateIndexValueWithUserDataTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint128 indexValue,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `indexValue` | uint128 |  |
| `userData` | bytes |  |

### distributeTest

```solidity
function distributeTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `amount` | uint256 |  |

### distributeWithUserDataTest

```solidity
function distributeWithUserDataTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    uint256 amount,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `amount` | uint256 |  |
| `userData` | bytes |  |

### approveSubscriptionTest

```solidity
function approveSubscriptionTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### approveSubscriptionWithUserDataTest

```solidity
function approveSubscriptionWithUserDataTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `userData` | bytes |  |

### revokeSubscriptionTest

```solidity
function revokeSubscriptionTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |

### revokeSubscriptionWithUserDataTest

```solidity
function revokeSubscriptionWithUserDataTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `userData` | bytes |  |

### updateSubscriptionUnitsTest

```solidity
function updateSubscriptionUnitsTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `units` | uint128 |  |

### updateSubscriptionUnitsWithUserDataTest

```solidity
function updateSubscriptionUnitsWithUserDataTest(
    contract ISuperfluidToken token,
    uint32 indexId,
    address subscriber,
    uint128 units,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `units` | uint128 |  |
| `userData` | bytes |  |

### deleteSubscriptionTest

```solidity
function deleteSubscriptionTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |

### deleteSubscriptionWithUserDataTest

```solidity
function deleteSubscriptionWithUserDataTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `userData` | bytes |  |

### claimTest

```solidity
function claimTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |

### claimWithUserDataTest

```solidity
function claimWithUserDataTest(
    contract ISuperfluidToken token,
    address publisher,
    uint32 indexId,
    address subscriber,
    bytes userData
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperfluidToken |  |
| `publisher` | address |  |
| `indexId` | uint32 |  |
| `subscriber` | address |  |
| `userData` | bytes |  |

# IDAv1LibrarySuperAppMock

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

### afterAgreementCreated

```solidity
function afterAgreementCreated(
    contract ISuperToken token,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(
    contract ISuperToken token,
    address ,
    bytes32 ,
    bytes ,
    bytes ,
    bytes ctx
) external returns (bytes newCtx)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken |  |
| `` | address |  |
| `` | bytes32 |  |
| `` | bytes |  |
| `` | bytes |  |
| `ctx` | bytes |  |

### _callbackTest

```solidity
function _callbackTest(
    contract ISuperToken token,
    bytes ctx
) internal returns (bytes)
```

extracts some user data to test out all callback library functions

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | super token |
| `ctx` | bytes | Context string |

#### Return Values

| Name | Type | Description |
| :--- | :--- | :---------- |
| `[0]` | bytes | New Context |

