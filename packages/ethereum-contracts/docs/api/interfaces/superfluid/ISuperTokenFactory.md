# ISuperTokenFactory

## Functions

### getHost

```solidity
function getHost(
) external returns (address host)
```

Get superfluid host contract address

### initialize

```solidity
function initialize(
) external
```

Initialize the contract

### getSuperTokenLogic

```solidity
function getSuperTokenLogic(
) external returns (contract ISuperToken superToken)
```

Get the current super token logic used by the factory

### createERC20Wrapper

```solidity
function createERC20Wrapper(
    contract IERC20 underlyingToken,
    uint8 underlyingDecimals,
    enum ISuperTokenFactory.Upgradability upgradability,
    string name,
    string symbol
) external returns (contract ISuperToken superToken)
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

## Events

### SuperTokenLogicCreated

```solidity
event SuperTokenLogicCreated(
    contract ISuperToken tokenLogic
)
```

Super token logic created event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `tokenLogic` | contract ISuperToken | Token logic address |
### SuperTokenCreated

```solidity
event SuperTokenCreated(
    contract ISuperToken token
)
```

Super token created event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | Newly created super token address |
### CustomSuperTokenCreated

```solidity
event CustomSuperTokenCreated(
    contract ISuperToken token
)
```

Custom super token created event

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `token` | contract ISuperToken | Newly created custom super token address |

