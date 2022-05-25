# SuperTokenFactoryBase

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host
) internal
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

### getHost

```solidity
function getHost(
) external returns (address host)
```

ISuperTokenFactory.getHost implementation

### initialize

```solidity
function initialize(
) external
```

Initialize the contract

### proxiableUUID

```solidity
function proxiableUUID(
) public returns (bytes32)
```

Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT.

### updateCode

```solidity
function updateCode(
    address newAddress
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `newAddress` | address |  |

### _updateSuperTokenLogic

```solidity
function _updateSuperTokenLogic(
) private
```

### getSuperTokenLogic

```solidity
function getSuperTokenLogic(
) external returns (contract ISuperToken)
```

Get the current super token logic used by the factory

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(
    contract ISuperfluid host
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

### createERC20Wrapper

```solidity
function createERC20Wrapper(
    contract IERC20 underlyingToken,
    uint8 underlyingDecimals,
    enum ISuperTokenFactory.Upgradability upgradability,
    string name,
    string symbol
) public returns (contract ISuperToken superToken)
```

Create new super token wrapper for the underlying ERC20 token

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `underlyingToken` | contract IERC20 | Underlying ERC20 token |
| `underlyingDecimals` | uint8 | Underlying token decimals |
| `upgradability` | enum ISuperTokenFactory.Upgradability | Upgradability mode |
| `name` | string | Super token name |
| `symbol` | string | Super token symbol |

### createERC20Wrapper

```solidity
function createERC20Wrapper(
    contract ERC20WithTokenInfo underlyingToken,
    enum ISuperTokenFactory.Upgradability upgradability,
    string name,
    string symbol
) external returns (contract ISuperToken superToken)
```

Create new super token wrapper for the underlying ERC20 token with extra token info

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `underlyingToken` | contract ERC20WithTokenInfo | Underlying ERC20 token |
| `upgradability` | enum ISuperTokenFactory.Upgradability | Upgradability mode |
| `name` | string | Super token name |
| `symbol` | string | Super token symbol

NOTE:
- It assumes token provide the .decimals() function |

### initializeCustomSuperToken

```solidity
function initializeCustomSuperToken(
    address customSuperTokenProxy
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `customSuperTokenProxy` | address |  |

# SuperTokenFactoryHelper

## Functions

### create

```solidity
function create(
    contract ISuperfluid host
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

# SuperTokenFactory

## Functions

### constructor

```solidity
function constructor(
    contract ISuperfluid host,
    contract SuperTokenFactoryHelper helper
) public
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `helper` | contract SuperTokenFactoryHelper |  |

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(
    contract ISuperfluid host
) external returns (address logic)
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |

