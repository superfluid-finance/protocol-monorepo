// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { TokenInfo } from "./TokenInfo.sol";


// Using abstract contract instead of interfaces because old solidity
// does not support interface inheriting other interfaces
// solhint-disable-next-line no-empty-blocks
abstract contract ERC20WithTokenInfo is IERC20, TokenInfo {}
