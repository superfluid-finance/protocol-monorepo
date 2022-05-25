# Solidity API

## SuperfluidGovernanceIIProxy

## SuperfluidGovernanceII

### _requireAuthorised

```solidity
function _requireAuthorised() private view
```

### proxiableUUID

```solidity
function proxiableUUID() public pure returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### updateCode

```solidity
function updateCode(address newAddress) external
```

### _requireAuthorised

```solidity
function _requireAuthorised(contract ISuperfluid) internal view
```

