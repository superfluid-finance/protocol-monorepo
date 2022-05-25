# Resolver

A simple implementation of IResolver using OZ AccessControl

NOTE:
Relevant events for indexing:
- OZ Access Control events `RoleGranted`/`RoleRevoked`: admin add/remove
- IResolver event `Set`: resolver name updates

## Functions

### constructor

```solidity
function constructor(
) public
```

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

