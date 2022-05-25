# Solidity API

## SuperTokenFactoryBase

### _host

```solidity
contract ISuperfluid _host
```

### _superTokenLogic

```solidity
contract ISuperToken _superTokenLogic
```

### constructor

```solidity
constructor(contract ISuperfluid host) internal
```

### getHost

```solidity
function getHost() external view returns (address host)
```

_ISuperTokenFactory.getHost implementation_

### initialize

```solidity
function initialize() external
```

_Initialize the contract_

### proxiableUUID

```solidity
function proxiableUUID() public pure returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### updateCode

```solidity
function updateCode(address newAddress) external
```

### _updateSuperTokenLogic

```solidity
function _updateSuperTokenLogic() private
```

### getSuperTokenLogic

```solidity
function getSuperTokenLogic() external view returns (contract ISuperToken)
```

_Get the current super token logic used by the factory_

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(contract ISuperfluid host) external virtual returns (address logic)
```

### createERC20Wrapper

```solidity
function createERC20Wrapper(contract IERC20 underlyingToken, uint8 underlyingDecimals, enum ISuperTokenFactory.Upgradability upgradability, string name, string symbol) public returns (contract ISuperToken superToken)
```

_Create new super token wrapper for the underlying ERC20 token_

| Name | Type | Description |
| ---- | ---- | ----------- |
| underlyingToken | contract IERC20 | Underlying ERC20 token |
| underlyingDecimals | uint8 | Underlying token decimals |
| upgradability | enum ISuperTokenFactory.Upgradability | Upgradability mode |
| name | string | Super token name |
| symbol | string | Super token symbol |

### createERC20Wrapper

```solidity
function createERC20Wrapper(contract ERC20WithTokenInfo underlyingToken, enum ISuperTokenFactory.Upgradability upgradability, string name, string symbol) external returns (contract ISuperToken superToken)
```

_Create new super token wrapper for the underlying ERC20 token with extra token info_

| Name | Type | Description |
| ---- | ---- | ----------- |
| underlyingToken | contract ERC20WithTokenInfo | Underlying ERC20 token |
| upgradability | enum ISuperTokenFactory.Upgradability | Upgradability mode |
| name | string | Super token name |
| symbol | string | Super token symbol NOTE: - It assumes token provide the .decimals() function |

### initializeCustomSuperToken

```solidity
function initializeCustomSuperToken(address customSuperTokenProxy) external
```

## SuperTokenFactoryHelper

### create

```solidity
function create(contract ISuperfluid host) external returns (address logic)
```

## SuperTokenFactory

### _helper

```solidity
contract SuperTokenFactoryHelper _helper
```

### constructor

```solidity
constructor(contract ISuperfluid host, contract SuperTokenFactoryHelper helper) public
```

### createSuperTokenLogic

```solidity
function createSuperTokenLogic(contract ISuperfluid host) external returns (address logic)
```

