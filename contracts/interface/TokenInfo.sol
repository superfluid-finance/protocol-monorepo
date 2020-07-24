pragma solidity >= 0.6.0;

interface TokenInfo {
    function name() external view returns (string memory);
    function symbol() external view returns (string memory);
    function decimals() external view returns (uint8);
}
