# Solidity API

## UUPSUtils

### _IMPLEMENTATION_SLOT

```solidity
bytes32 _IMPLEMENTATION_SLOT
```

_Implementation slot constant.
Using https://eips.ethereum.org/EIPS/eip-1967 standard
Storage slot 0x360894a13ba1a3210667c828492db98dca3e2076cc3735a920a3ca505d382bbc
(obtained as bytes32(uint256(keccak256(&#x27;eip1967.proxy.implementation&#x27;)) - 1))._

### implementation

```solidity
function implementation() internal view returns (address impl)
```

_Get implementation address._

### setImplementation

```solidity
function setImplementation(address codeAddress) internal
```

_Set new implementation address._

