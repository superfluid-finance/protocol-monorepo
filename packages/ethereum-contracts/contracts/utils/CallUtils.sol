// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;


/**
 * @dev Call utilities that is absent from the OpenZeppelin
 */
library CallUtils {

    /// @dev Get the revert message from a call
    /// @notice This is needed in order to get the human-readable revert message from a call
    /// @param res Response of the call
    /// @return Revert message string
    function getRevertMsg(bytes memory res) internal pure returns (string memory) {
        // If the _res length is less than 68, then the transaction failed silently (without a revert message)
        if (res.length < 68) return "CallUtils: target reverted";
        // solhint-disable-next-line no-inline-assembly
        assembly {
            // Slice the sighash.
            res := add(res, 0x04)
        }
        return abi.decode(res, (string)); // All that remains is the revert string
    }

    /**
    * @dev Helper method to parse data and extract the method signature (selector).
    *
    * Copied from: https://github.com/argentlabs/argent-contracts/
    * blob/master/contracts/modules/common/Utils.sol#L54-L60
    */
    function parseSelector(bytes memory callData) internal pure returns (bytes4 selector) {
        require(callData.length >= 4, "CallUtils: invalid callData");
        // solhint-disable-next-line no-inline-assembly
        assembly {
            selector := mload(add(callData, 0x20))
        }
    }

    /**
     * @dev Pad length to 32 bytes word boundary
     */
    function padLength32(uint256 len) internal pure returns (uint256 paddedLen) {
        return ((len / 32) +  (((len & 31) > 0) /* rounding? */ ? 1 : 0)) * 32;
    }

    /**
     * @dev It the data encoded correctly with abi.encode(bytesData)
     *
     * Expected ABI Encode Layout:
     * | word 1      | word 2           | word 3           | the rest...
     * | data length | bytesData offset | bytesData length | bytesData + padLength32 zeros |
     */
    function isValidAbiEncodedBytes(bytes memory data) internal pure returns (bool) {
        if (data.length < 64) return false;
        uint bytesOffset;
        uint bytesLen;
        // bytes offset is always expected to be 32
        assembly { bytesOffset := mload(add(data, 32)) }
        if (bytesOffset != 32) return false;
        assembly { bytesLen := mload(add(data, 64)) }
        // the data length should be bytesData.length + 64 + padded bytes length
        return data.length == 64 + padLength32(bytesLen);
    }

}
