// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";


interface IChiToken is IERC20 {
    function computeAddress2(uint256 salt) external view returns (address child);
    function mint(uint256 value) external;
    function free(uint256 value) external returns (uint256);
    function freeUpTo(uint256 value) external returns (uint256);
    function freeFrom(address from, uint256 value) external returns (uint256);
    function freeFromUpTo(address from, uint256 value) external returns (uint256);
}
