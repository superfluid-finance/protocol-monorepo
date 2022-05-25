# Solidity API

## UUPSProxiable

### getCodeAddress

```solidity
function getCodeAddress() public view returns (address codeAddress)
```

_Get current implementation code address._

### updateCode

```solidity
function updateCode(address newAddress) external virtual
```

### proxiableUUID

```solidity
function proxiableUUID() public view virtual returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### _updateCodeAddress

```solidity
function _updateCodeAddress(address newAddress) internal
```

_Update code address function.
     It is internal, so the derived contract could setup its own permission logic._

### CodeUpdated

```solidity
event CodeUpdated(bytes32 uuid, address codeAddress)
```

