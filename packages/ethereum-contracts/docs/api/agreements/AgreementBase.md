# Solidity API

## AgreementBase

### _host

```solidity
address _host
```

### constructor

```solidity
constructor(address host) internal
```

### proxiableUUID

```solidity
function proxiableUUID() public view returns (bytes32)
```

_Proxiable UUID marker function, this would help to avoid wrong logic
     contract to be used for upgrading.

NOTE: The semantics of the UUID deviates from the actual UUPS standard,
      where it is equivalent of _IMPLEMENTATION_SLOT._

### updateCode

```solidity
function updateCode(address newAddress) external
```

