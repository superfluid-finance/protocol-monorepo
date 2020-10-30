// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;


library SafeMathExtra {

    function downcastINT256(uint256 a) internal pure returns(int256 b) {
        require(a <= uint256(type(int256).max), "SafeMathExtra: uint256 -> int256 overflow");
        return int256(a);
    }

}
