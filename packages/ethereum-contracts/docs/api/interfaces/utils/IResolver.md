# IResolver

## Functions

### set

```solidity
function set(
    string name,
    address target
) external
```

Set resolver address name

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `name` | string |  |
| `target` | address |  |

### get

```solidity
function get(
    string name
) external returns (address)
```

Get address by name

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `name` | string |  |

## Events

### Set

```solidity
event Set(
    string name,
    address target
)
```

#### Parameters:

| Name | Type | Description |
| :--- | :--- | :---------- |
| `name` | string |  |
| `target` | address |  |

