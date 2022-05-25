# ISuperfluidGovernance

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

### getConfigAsAddress

```solidity
function getConfigAsAddress(
    contract ISuperfluid host,
    contract ISuperfluidToken superToken,
    bytes32 key
) external returns (address value)
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
) external returns (uint256 value)
```

Get configuration as uint256 value

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `superToken` | contract ISuperfluidToken |  |
| `key` | bytes32 |  |

