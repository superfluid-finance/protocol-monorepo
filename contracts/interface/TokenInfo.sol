// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

interface TokenInfo {
    function name() external view returns (string memory);
    function symbol() external view returns (string memory);
    function decimals() external view returns (uint8);
}
