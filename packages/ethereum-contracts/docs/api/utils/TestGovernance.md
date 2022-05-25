# TestGovernance

A initializable version of the governance for testing purpose

## Functions

### initialize

```solidity
function initialize(
    contract ISuperfluid host,
    address rewardAddress,
    uint256 liquidationPeriod,
    uint256 patricianPeriod,
    address[] trustedForwarders
) external
```

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | contract ISuperfluid |  |
| `rewardAddress` | address |  |
| `liquidationPeriod` | uint256 |  |
| `patricianPeriod` | uint256 |  |
| `trustedForwarders` | address[] |  |

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

