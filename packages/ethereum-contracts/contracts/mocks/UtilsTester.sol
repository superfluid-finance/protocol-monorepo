// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { Int96SafeMath } from "../libs/Int96SafeMath.sol";
import { UInt128SafeMath } from "../libs/UInt128SafeMath.sol";


contract UtilsTester {

    /// Int96SafeMath

    function getInt96Max() external pure returns (int96) {
        return type(int96).max;
    }

    function getInt96Min() external pure returns (int96) {
        return type(int96).min;
    }

    function echoInt96(int96 a) external pure returns (int96) {
        int96 c1 = Int96SafeMath.add(a, 0, "echoInt96 overflow");
        int96 c2 = Int96SafeMath.sub(a, 0, "echoInt96 overflow");
        assert(c1 == c2);
        return c1;
    }

    function doInt96SafeMathMul(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.mul(a, b, "doInt96SafeMathMul overflow");
    }

    function doInt96SafeMathAdd(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.add(a, b, "doInt96SafeMathAdd overflow");
    }

    function doInt96SafeMathSub(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.sub(a, b, "doInt96SafeMathSub overflow");
    }

    function doInt96SafeMathDiv(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.div(a, b, "doInt96SafeMathDiv overflow");
    }

    /// UInt128SafeMath

    function getUint128Max() external pure returns (uint128) {
        return type(uint128).max;
    }

    function doUInt128SafeMathAdd(uint128 a, uint128 b) external pure returns (uint128 c) {
        return UInt128SafeMath.add(a, b, "doUInt128SafeMathAdd overflow");
    }

    function doInt128SafeMathSub(uint128 a, uint128 b) external pure returns (uint128 c) {
        return UInt128SafeMath.sub(a, b, "doInt96SafeMathSub overflow");
    }

}
