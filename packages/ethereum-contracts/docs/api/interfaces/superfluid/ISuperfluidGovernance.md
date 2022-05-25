# Solidity API

## ISuperfluidGovernance

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

### getConfigAsAddress

```solidity
function getConfigAsAddress(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) external view returns (address value)
```

_Get configuration as address value_

### getConfigAsUint256

```solidity
function getConfigAsUint256(contract ISuperfluid host, contract ISuperfluidToken superToken, bytes32 key) external view returns (uint256 value)
```

_Get configuration as uint256 value_

