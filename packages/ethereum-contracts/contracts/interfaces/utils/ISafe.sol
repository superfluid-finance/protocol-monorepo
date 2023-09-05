// SPDX-License-Identifier: MIT
pragma solidity >=0.8.11;

// minimal interface for interacting with Safe contracts
interface ISafe {
    function nonce() external view returns (uint256);
    
    // solhint-disable-next-line func-name-mixedcase
    function VERSION() external view returns (string memory);

    function getTransactionHash(
        address to,
        uint256 value,
        bytes calldata data,
        uint8 operation,
        uint256 safeTxGas,
        uint256 baseGas,
        uint256 gasPrice,
        address gasToken,
        address refundReceiver,
        uint256 nonce
    ) external view returns (bytes32);
}
