# Solidity API

## SuperfluidGovernanceBase

### Value

```solidity
struct Value {
  bool set;
  uint256 value;
}
```

### _configs

```solidity
mapping(address &#x3D;&gt; mapping(address &#x3D;&gt; mapping(bytes32 &#x3D;&gt; struct SuperfluidGovernanceBase.Value))) _configs
```

### replaceGovernance

```solidity
function replaceGovernance(contract ISuperfluid host, address newGov) external
```

_Replace the current governance with a new governance_

### registerAgreementClass

```solidity
function registerAgreementClass(contract ISuperfluid host, address agreementClass) external
```

_Register a new agreement class_

### updateContracts

```solidity
function updateContracts(contract ISuperfluid host, address hostNewLogic, address[] agreementClassNewLogics, address superTokenFactoryNewLogic) external
```

_Update logics of the contracts

NOTE:
- Because they might have inter-dependencies, it is good to have one single function to update them all_

### batchUpdateSuperTokenLogic

```solidity
function batchUpdateSuperTokenLogic(contract ISuperfluid host, contract ISuperToken[] tokens) external
```

_Update supertoken logic contract to the latest that is managed by the super token factory_

### batchUpdateSuperTokenMinimumDeposit

```solidity
function batchUpdateSuperTokenMinimumDeposit(contract ISuperfluid host, contract ISuperToken[] tokens, uint256[] minimumDeposits) external
```

### ConfigChanged

```solidity
event ConfigChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key, bool isKeySet, uint256 value)
```

### setConfig

```solidity
function setConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key, address value) external
```

_Set configuration as address value_

### setConfig

```solidity
function setConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key, uint256 value) external
```

_Set configuration as uint256 value_

### clearConfig

```solidity
function clearConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) external
```

_Clear configuration_

### _setConfig

```solidity
function _setConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key, address value) internal
```

### _setConfig

```solidity
function _setConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key, uint256 value) internal
```

### _clearConfig

```solidity
function _clearConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) internal
```

### getConfigAsAddress

```solidity
function getConfigAsAddress(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) public view returns (address value)
```

_Get configuration as address value_

### getConfigAsUint256

```solidity
function getConfigAsUint256(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) public view returns (uint256 period)
```

_Get configuration as uint256 value_

### RewardAddressChanged

```solidity
event RewardAddressChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bool isKeySet, address rewardAddress)
```

### getRewardAddress

```solidity
function getRewardAddress(contract ISuperfluid host, contract ISuperfluidToken superToken) public view returns (address)
```

### setRewardAddress

```solidity
function setRewardAddress(contract ISuperfluid host, contract ISuperfluidToken superToken, address rewardAddress) public
```

### clearRewardAddress

```solidity
function clearRewardAddress(contract ISuperfluid host, contract ISuperfluidToken superToken) public
```

### CFAv1LiquidationPeriodChanged

```solidity
event CFAv1LiquidationPeriodChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bool isKeySet, uint256 liquidationPeriod)
```

### PPPConfigurationChanged

```solidity
event PPPConfigurationChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bool isKeySet, uint256 liquidationPeriod, uint256 patricianPeriod)
```

### getPPPConfig

```solidity
function getPPPConfig(contract ISuperfluid host, contract ISuperfluidToken superToken) public view returns (uint256 liquidationPeriod, uint256 patricianPeriod)
```

### setPPPConfig

```solidity
function setPPPConfig(contract ISuperfluid host, contract ISuperfluidToken superToken, uint256 liquidationPeriod, uint256 patricianPeriod) public
```

### clearPPPConfig

```solidity
function clearPPPConfig(contract ISuperfluid host, contract ISuperfluidToken superToken) public
```

### SuperTokenMinimumDepositChanged

```solidity
event SuperTokenMinimumDepositChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bool isKeySet, uint256 minimumDeposit)
```

### getSuperTokenMinimumDeposit

```solidity
function getSuperTokenMinimumDeposit(contract ISuperfluid host, contract ISuperfluidToken superToken) public view returns (uint256 value)
```

### setSuperTokenMinimumDeposit

```solidity
function setSuperTokenMinimumDeposit(contract ISuperfluid host, contract ISuperfluidToken superToken, uint256 value) public
```

### clearSuperTokenMinimumDeposit

```solidity
function clearSuperTokenMinimumDeposit(contract ISuperfluid host, contract ISuperToken superToken) public
```

### TrustedForwarderChanged

```solidity
event TrustedForwarderChanged(contract ISuperfluid host, contract ISuperfluidToken superToken, bool isKeySet, address forwarder, bool enabled)
```

### isTrustedForwarder

```solidity
function isTrustedForwarder(contract ISuperfluid host, contract ISuperfluidToken superToken, address forwarder) public view returns (bool)
```

### enableTrustedForwarder

```solidity
function enableTrustedForwarder(contract ISuperfluid host, contract ISuperfluidToken superToken, address forwarder) public
```

### disableTrustedForwarder

```solidity
function disableTrustedForwarder(contract ISuperfluid host, contract ISuperfluidToken superToken, address forwarder) public
```

### clearTrustedForwarder

```solidity
function clearTrustedForwarder(contract ISuperfluid host, contract ISuperfluidToken superToken, address forwarder) public
```

### isAuthorizedAppFactory

```solidity
function isAuthorizedAppFactory(contract ISuperfluid host, address factory) public view returns (bool)
```

_tells if the given factory is authorized to register apps_

### authorizeAppFactory

```solidity
function authorizeAppFactory(contract ISuperfluid host, address factory) public
```

_allows the given factory to register new apps without requiring onetime keys_

| Name | Type | Description |
| ---- | ---- | ----------- |
| host | contract ISuperfluid |  |
| factory | address | must be an initialized contract |

### unauthorizeAppFactory

```solidity
function unauthorizeAppFactory(contract ISuperfluid host, address factory) public
```

_withdraws authorization from a factory to register new apps.
Doesn&#x27;t affect apps previously registered by the factory._

### onlyAuthorized

```solidity
modifier onlyAuthorized(contract ISuperfluid host)
```

### _requireAuthorised

```solidity
function _requireAuthorised(contract ISuperfluid host) internal view virtual
```

