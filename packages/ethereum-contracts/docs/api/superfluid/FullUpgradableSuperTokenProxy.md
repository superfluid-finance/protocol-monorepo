# Solidity API

## FullUpgradableSuperTokenProxy

### _FACTORY_SLOT

```solidity
bytes32 _FACTORY_SLOT
```

### initialize

```solidity
function initialize() external
```

### _implementation

```solidity
function _implementation() internal view returns (address impl)
```

_This is a virtual function that should be overriden so it returns the address to which the fallback function
and {_fallback} should delegate._

