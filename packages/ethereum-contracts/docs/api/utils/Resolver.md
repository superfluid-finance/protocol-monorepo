# Solidity API

## Resolver

_A simple implementation of IResolver using OZ AccessControl

NOTE:
Relevant events for indexing:
- OZ Access Control events &#x60;RoleGranted&#x60;/&#x60;RoleRevoked&#x60;: admin add/remove
- IResolver event &#x60;Set&#x60;: resolver name updates_

### _registry

```solidity
mapping(string &#x3D;&gt; address) _registry
```

### constructor

```solidity
constructor() public
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

