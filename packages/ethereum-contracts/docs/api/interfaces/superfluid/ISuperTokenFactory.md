# Solidity API

## ISuperTokenFactory

### getHost

```solidity
function getHost() external view returns (address host)
```

_Get superfluid host contract address_

### initialize

```solidity
function initialize() external
```

_Initialize the contract_

### getSuperTokenLogic

```solidity
function getSuperTokenLogic() external view returns (contract ISuperToken superToken)
```

_Get the current super token logic used by the factory_

### Upgradability

```solidity
enum Upgradability {
  NON_UPGRADABLE,
  SEMI_UPGRADABLE,
  FULL_UPGRADABE
}
```

### createERC20Wrapper

```solidity
function createERC20Wrapper(contract IERC20 underlyingToken, uint8 underlyingDecimals, enum ISuperTokenFactory.Upgradability upgradability, string name, string symbol) external returns (contract ISuperToken superToken)
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

### SuperTokenLogicCreated

```solidity
event SuperTokenLogicCreated(contract ISuperToken tokenLogic)
```

_Super token logic created event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| tokenLogic | contract ISuperToken | Token logic address |

### SuperTokenCreated

```solidity
event SuperTokenCreated(contract ISuperToken token)
```

_Super token created event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | Newly created super token address |

### CustomSuperTokenCreated

```solidity
event CustomSuperTokenCreated(contract ISuperToken token)
```

_Custom super token created event_

| Name | Type | Description |
| ---- | ---- | ----------- |
| token | contract ISuperToken | Newly created custom super token address |

