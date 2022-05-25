# SuperfluidFrameworkDeployer

This is NOT for deploying public nets, but rather only for tesing envs

## Functions

### constructor

```solidity
function constructor(
) public
```

Deploys everything... probably

### getFramework

```solidity
function getFramework(
) external returns (struct SuperfluidFrameworkDeployer.Framework sf)
```

Fetches the framework contracts

### deployWrapperSuperToken

```solidity
function deployWrapperSuperToken(
    string name,
    string symbol
) external returns (contract ERC20PresetMinterPauser token, contract SuperToken superToken)
```

Deploy new wrapper super token

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `name` | string |  |
| `symbol` | string |  |

