# Solidity API

## FixedSizeData

### storeData

```solidity
function storeData(bytes32 slot, bytes32[] data) internal
```

_Store data to the slot at &#x60;slot&#x60;_

### hasData

```solidity
function hasData(bytes32 slot, uint256 dataLength) internal view returns (bool)
```

### loadData

```solidity
function loadData(bytes32 slot, uint256 dataLength) internal view returns (bytes32[] data)
```

_Load data of size &#x60;dataLength&#x60; from the slot at &#x60;slot&#x60;_

### eraseData

```solidity
function eraseData(bytes32 slot, uint256 dataLength) internal
```

_Erase data of size &#x60;dataLength&#x60; from the slot at &#x60;slot&#x60;_

