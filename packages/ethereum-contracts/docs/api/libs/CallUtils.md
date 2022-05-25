# CallUtils

## Functions

### revertFromReturnedData

```solidity
function revertFromReturnedData(
    bytes returnedData
) internal
```

This is needed in order to provide some human-readable revert message from a call

Bubble up the revert from the returnedData (supports Panic, Error & Custom Errors)

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `returnedData` | bytes | Response of the call |

### parseSelector

```solidity
function parseSelector(
    bytes callData
) internal returns (bytes4 selector)
```

Helper method to parse data and extract the method signature (selector).

Copied from: https://github.com/argentlabs/argent-contracts/
blob/master/contracts/modules/common/Utils.sol#L54-L60

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `callData` | bytes |  |

### padLength32

```solidity
function padLength32(
    uint256 len
) internal returns (uint256 paddedLen)
```

Pad length to 32 bytes word boundary

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `len` | uint256 |  |

### isValidAbiEncodedBytes

```solidity
function isValidAbiEncodedBytes(
    bytes data
) internal returns (bool)
```

Validate if the data is encoded correctly with abi.encode(bytesData)

Expected ABI Encode Layout:
| word 1      | word 2           | word 3           | the rest...
| data length | bytesData offset | bytesData length | bytesData + padLength32 zeros |

#### Parameters

| Name | Type | Description |
| :--- | :--- | :---------- |
| `data` | bytes |  |

