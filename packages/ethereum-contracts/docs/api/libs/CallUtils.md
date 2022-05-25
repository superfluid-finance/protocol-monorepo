# Solidity API

## CallUtils

### revertFromReturnedData

```solidity
function revertFromReturnedData(bytes returnedData) internal pure
```

This is needed in order to provide some human-readable revert message from a call

_Bubble up the revert from the returnedData (supports Panic, Error &amp; Custom Errors)_

| Name | Type | Description |
| ---- | ---- | ----------- |
| returnedData | bytes | Response of the call |

### parseSelector

```solidity
function parseSelector(bytes callData) internal pure returns (bytes4 selector)
```

_Helper method to parse data and extract the method signature (selector).

Copied from: https://github.com/argentlabs/argent-contracts/
blob/master/contracts/modules/common/Utils.sol#L54-L60_

### padLength32

```solidity
function padLength32(uint256 len) internal pure returns (uint256 paddedLen)
```

_Pad length to 32 bytes word boundary_

### isValidAbiEncodedBytes

```solidity
function isValidAbiEncodedBytes(bytes data) internal pure returns (bool)
```

_Validate if the data is encoded correctly with abi.encode(bytesData)

Expected ABI Encode Layout:
| word 1      | word 2           | word 3           | the rest...
| data length | bytesData offset | bytesData length | bytesData + padLength32 zeros |_

