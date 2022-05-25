# Solidity API

## IDAv1LibraryMock

### _idaLib

```solidity
struct IDAv1Library.InitData _idaLib
```

### _IDAV1_HASH

```solidity
bytes32 _IDAV1_HASH
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### getIndexTest

```solidity
function getIndexTest(contract ISuperfluidToken token, address publisher, uint32 indexId) external view returns (bool exist, uint128 indexValue, uint128 totalUnitsApproved, uint128 totalUnitsPending)
```

### calculateDistributionTest

```solidity
function calculateDistributionTest(contract ISuperfluidToken token, address publisher, uint32 indexId, uint256 amount) external view returns (uint256 actualAmount, uint128 newIndexValue)
```

### listSubscriptionsTest

```solidity
function listSubscriptionsTest(contract ISuperfluidToken token, address subscriber) external view returns (address[] publishers, uint32[] indexIds, uint128[] unitsList)
```

### getSubscriptionTest

```solidity
function getSubscriptionTest(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber) external view returns (bool exist, bool approved, uint128 units, uint256 pendingDistribution)
```

### getSubscriptionByIDTest

```solidity
function getSubscriptionByIDTest(contract ISuperfluidToken token, bytes32 agreementId) external view returns (address publisher, uint32 indexId, bool approved, uint128 units, uint256 pendingDistribution)
```

_agreementId &#x3D;&#x3D; keccak256(abi.encodePacked(&quot;subscription&quot;, subscriber, indexId));_

### createIndexTest

```solidity
function createIndexTest(contract ISuperfluidToken token, uint32 indexId) external
```

### createIndexWithUserDataTest

```solidity
function createIndexWithUserDataTest(contract ISuperfluidToken token, uint32 indexId, bytes userData) external
```

### updateIndexValueTest

```solidity
function updateIndexValueTest(contract ISuperfluidToken token, uint32 indexId, uint128 indexValue) external
```

### updateIndexValueWithUserDataTest

```solidity
function updateIndexValueWithUserDataTest(contract ISuperfluidToken token, uint32 indexId, uint128 indexValue, bytes userData) external
```

### distributeTest

```solidity
function distributeTest(contract ISuperfluidToken token, uint32 indexId, uint256 amount) external
```

### distributeWithUserDataTest

```solidity
function distributeWithUserDataTest(contract ISuperfluidToken token, uint32 indexId, uint256 amount, bytes userData) external
```

### approveSubscriptionTest

```solidity
function approveSubscriptionTest(contract ISuperfluidToken token, address publisher, uint32 indexId) external
```

### approveSubscriptionWithUserDataTest

```solidity
function approveSubscriptionWithUserDataTest(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes userData) external
```

### revokeSubscriptionTest

```solidity
function revokeSubscriptionTest(contract ISuperfluidToken token, address publisher, uint32 indexId) external
```

### revokeSubscriptionWithUserDataTest

```solidity
function revokeSubscriptionWithUserDataTest(contract ISuperfluidToken token, address publisher, uint32 indexId, bytes userData) external
```

### updateSubscriptionUnitsTest

```solidity
function updateSubscriptionUnitsTest(contract ISuperfluidToken token, uint32 indexId, address subscriber, uint128 units) external
```

### updateSubscriptionUnitsWithUserDataTest

```solidity
function updateSubscriptionUnitsWithUserDataTest(contract ISuperfluidToken token, uint32 indexId, address subscriber, uint128 units, bytes userData) external
```

### deleteSubscriptionTest

```solidity
function deleteSubscriptionTest(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber) external
```

### deleteSubscriptionWithUserDataTest

```solidity
function deleteSubscriptionWithUserDataTest(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes userData) external
```

### claimTest

```solidity
function claimTest(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber) external
```

### claimWithUserDataTest

```solidity
function claimWithUserDataTest(contract ISuperfluidToken token, address publisher, uint32 indexId, address subscriber, bytes userData) external
```

## IDAv1LibrarySuperAppMock

### _MOCK_USER_DATA

```solidity
bytes _MOCK_USER_DATA
```

### constructor

```solidity
constructor(contract ISuperfluid host) public
```

### afterAgreementCreated

```solidity
function afterAgreementCreated(contract ISuperToken token, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

### afterAgreementUpdated

```solidity
function afterAgreementUpdated(contract ISuperToken token, address, bytes32, bytes, bytes, bytes ctx) external returns (bytes newCtx)
```

### FunctionIndex

```solidity
enum FunctionIndex {
  CREATE_INDEX,
  CREATE_INDEX_USER_DATA,
  UPDATE_INDEX,
  UPDATE_INDEX_USER_DATA,
  DISTRIBUTE,
  DISTRIBUTE_USER_DATA,
  APROVE_SUBSCRIPTION,
  APROVE_SUBSCRIPTION_USER_DATA,
  REVOKE_SUBSCRIPTION,
  REVOKE_SUBSCRIPTION_USER_DATA,
  UPDATE_SUBSCRIPTION,
  UPDATE_SUBSCRIPTION_USER_DATA,
  DELETE_SUBSCRIPTION,
  DELETE_SUBSCRIPTION_USER_DATA,
  CLAIM,
  CLAIM_USER_DATA
}
```

### _callbackTest

```solidity
function _callbackTest(contract ISuperToken token, bytes ctx) internal returns (bytes)
```

_extracts some user data to test out all callback library functions_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | super token |
| ctx | bytes | Context string |

| Name | Type | Description |
| ---- | ---- | ----------- |
| [0] | bytes | New Context |

