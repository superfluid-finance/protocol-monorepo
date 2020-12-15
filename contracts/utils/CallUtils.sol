// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;


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
        if (res.length < 68) return "SF: target reverted";
        // solhint-disable-next-line no-inline-assembly
        assembly {
            // Slice the sighash.
            res := add(res, 0x04)
        }
        return abi.decode(res, (string)); // All that remains is the revert string
    }

    /**
    * @notice Helper method to parse data and extract the method signature.
    *
    * Copied from: https://github.com/argentlabs/argent-contracts/
    * blob/master/contracts/modules/common/Utils.sol#L54-L60
    */
    function functionPrefix(bytes memory data) internal pure returns (bytes4 prefix) {
        require(data.length >= 4, "SF: invalid functionPrefix");
        // solhint-disable-next-line no-inline-assembly
        assembly {
            prefix := mload(add(data, 0x20))
        }
    }

}
