# Solidity API

## IResolver

### Set

```solidity
event Set(string name, address target)
```

### set

```solidity
function set(string name, address target) external
```

_Set resolver address name_

### get

```solidity
function get(string name) external view returns (address)
```

_Get address by name_

