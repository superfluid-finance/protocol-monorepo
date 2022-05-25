# BatchLiquidator

## Functions

### deleteFlows

```solidity
function deleteFlows(
    address host,
    address cfa,
    address superToken,
    address[] senders,
    address[] receivers
) external
```

Delete flows in batch

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `host` | address | - The host contract address. |
| `cfa` | address | - The cfa contract address. |
| `superToken` | address | - The super token the flows belong to. |
| `senders` | address[] | - List of senders. |
| `receivers` | address[] | - Corresponding list of receivers. |

