# SuperfluidGovernanceBase

## Functions

### replaceGovernance

```solidity
function replaceGovernance(
    contract ISuperfluid host,
    address newGov
) external
```

Replace the current governance with a new governance

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `newGov` | address |  |

### registerAgreementClass

```solidity
function registerAgreementClass(
    contract ISuperfluid host,
    address agreementClass
) external
```

Register a new agreement class

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `agreementClass` | address |  |

### updateContracts

```solidity
function updateContracts(
    contract ISuperfluid host,
    address hostNewLogic,
    address[] agreementClassNewLogics,
    address superTokenFactoryNewLogic
) external
```

Update logics of the contracts

NOTE:
- Because they might have inter-dependencies, it is good to have one single function to update them all

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `hostNewLogic` | address |  |
| `agreementClassNewLogics` | address[] |  |
| `superTokenFactoryNewLogic` | address |  |

### batchUpdateSuperTokenLogic

```solidity
function batchUpdateSuperTokenLogic(
    contract ISuperfluid host,
    contract ISuperToken[] tokens
) external
```

Update supertoken logic contract to the latest that is managed by the super token factory

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `tokens` | contract ISuperToken[] |  |

### batchUpdateSuperTokenMinimumDeposit

```solidity
function batchUpdateSuperTokenMinimumDeposit(
    contract ISuperfluid host,
    contract ISuperToken[] tokens,
    uint256[] minimumDeposits
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `tokens` | contract ISuperToken[] |  |
| `minimumDeposits` | uint256[] |  |

### setConfig

```solidity
function setConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key,
    address value
) external
```

Set configuration as address value

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |
| `value` | address |  |

### setConfig

```solidity
function setConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key,
    uint256 value
) external
```

Set configuration as uint256 value

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |
| `value` | uint256 |  |

### clearConfig

```solidity
function clearConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key
) external
```

Clear configuration

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |

### _setConfig

```solidity
function _setConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key,
    address value
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |
| `value` | address |  |

### _setConfig

```solidity
function _setConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key,
    uint256 value
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |
| `value` | uint256 |  |

### _clearConfig

```solidity
function _clearConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |

### getConfigAsAddress

```solidity
function getConfigAsAddress(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key
) public returns (address value)
```

Get configuration as address value

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |

### getConfigAsUint256

```solidity
function getConfigAsUint256(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key
) public returns (uint256 period)
```

Get configuration as uint256 value

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |

### getRewardAddress

```solidity
function getRewardAddress(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken
) public returns (address)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |

### setRewardAddress

```solidity
function setRewardAddress(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    address rewardAddress
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `rewardAddress` | address |  |

### clearRewardAddress

```solidity
function clearRewardAddress(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |

### getPPPConfig

```solidity
function getPPPConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken
) public returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |

### setPPPConfig

```solidity
function setPPPConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    uint256 liquidationPeriod,
    uint256 patricianPeriod
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `liquidationPeriod` | uint256 |  |
| `patricianPeriod` | uint256 |  |

### clearPPPConfig

```solidity
function clearPPPConfig(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |

### getSuperTokenMinimumDeposit

```solidity
function getSuperTokenMinimumDeposit(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken
) public returns (uint256 value)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |

### setSuperTokenMinimumDeposit

```solidity
function setSuperTokenMinimumDeposit(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    uint256 value
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `value` | uint256 |  |

### clearSuperTokenMinimumDeposit

```solidity
function clearSuperTokenMinimumDeposit(
    contract ISuperfluid host,
    contract ISuperToken superToken
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperToken |  |

### isTrustedForwarder

```solidity
function isTrustedForwarder(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    address forwarder
) public returns (bool)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `forwarder` | address |  |

### enableTrustedForwarder

```solidity
function enableTrustedForwarder(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    address forwarder
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `forwarder` | address |  |

### disableTrustedForwarder

```solidity
function disableTrustedForwarder(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    address forwarder
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `forwarder` | address |  |

### clearTrustedForwarder

```solidity
function clearTrustedForwarder(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    address forwarder
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `forwarder` | address |  |

### isAuthorizedAppFactory

```solidity
function isAuthorizedAppFactory(
    contract ISuperfluid host,
    address factory
) public returns (bool)
```

tells if the given factory is authorized to register apps

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `factory` | address |  |

### authorizeAppFactory

```solidity
function authorizeAppFactory(
    contract ISuperfluid host,
    address factory
) public
```

allows the given factory to register new apps without requiring onetime keys

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `factory` | address | must be an initialized contract |

### unauthorizeAppFactory

```solidity
function unauthorizeAppFactory(
    contract ISuperfluid host,
    address factory
) public
```

withdraws authorization from a factory to register new apps.
Doesn't affect apps previously registered by the factory.

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `factory` | address |  |

### _requireAuthorised

```solidity
function _requireAuthorised(
    contract ISuperfluid host
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

## Events

### ConfigChanged

```solidity
event ConfigChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key,
    bool isKeySet,
    uint256 value
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |
| `isKeySet` | bool |  |
| `value` | uint256 |  |
### RewardAddressChanged

```solidity
event RewardAddressChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bool isKeySet,
    address rewardAddress
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `isKeySet` | bool |  |
| `rewardAddress` | address |  |
### CFAv1LiquidationPeriodChanged

```solidity
event CFAv1LiquidationPeriodChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bool isKeySet,
    uint256 liquidationPeriod
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `isKeySet` | bool |  |
| `liquidationPeriod` | uint256 |  |
### PPPConfigurationChanged

```solidity
event PPPConfigurationChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bool isKeySet,
    uint256 liquidationPeriod,
    uint256 patricianPeriod
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `isKeySet` | bool |  |
| `liquidationPeriod` | uint256 |  |
| `patricianPeriod` | uint256 |  |
### SuperTokenMinimumDepositChanged

```solidity
event SuperTokenMinimumDepositChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bool isKeySet,
    uint256 minimumDeposit
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `isKeySet` | bool |  |
| `minimumDeposit` | uint256 |  |
### TrustedForwarderChanged

```solidity
event TrustedForwarderChanged(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bool isKeySet,
    address forwarder,
    bool enabled
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `isKeySet` | bool |  |
| `forwarder` | address |  |
| `enabled` | bool |  |

