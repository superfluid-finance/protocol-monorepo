# Solidity API

## UUPSProxiableMock

### _uuid

```solidity
bytes32 _uuid
```

### waterMark

```solidity
uint256 waterMark
```

### constructor

```solidity
constructor(bytes32 uuid, uint256 w) public
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

