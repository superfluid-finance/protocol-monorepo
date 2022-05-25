# Solidity API

## ERC777Helper

### _ERC1820_REGISTRY

```solidity
contract IERC1820Registry _ERC1820_REGISTRY
```

### _TOKENS_SENDER_INTERFACE_HASH

```solidity
bytes32 _TOKENS_SENDER_INTERFACE_HASH
```

### _TOKENS_RECIPIENT_INTERFACE_HASH

```solidity
bytes32 _TOKENS_RECIPIENT_INTERFACE_HASH
```

### Operators

```solidity
struct Operators {
  address[] defaultOperatorsArray;
  mapping(address &#x3D;&gt; bool) defaultOperators;
  mapping(address &#x3D;&gt; mapping(address &#x3D;&gt; bool)) operators;
  mapping(address &#x3D;&gt; mapping(address &#x3D;&gt; bool)) revokedDefaultOperators;
}
```

### register

```solidity
function register(address token) internal
```

### isOperatorFor

```solidity
function isOperatorFor(struct ERC777Helper.Operators self, address operator, address tokenHolder) internal view returns (bool)
```

### authorizeOperator

```solidity
function authorizeOperator(struct ERC777Helper.Operators self, address holder, address operator) internal
```

### revokeOperator

```solidity
function revokeOperator(struct ERC777Helper.Operators self, address holder, address operator) internal
```

### defaultOperators

```solidity
function defaultOperators(struct ERC777Helper.Operators self) internal view returns (address[])
```

### setupDefaultOperators

```solidity
function setupDefaultOperators(struct ERC777Helper.Operators self, address[] operators) internal
```

