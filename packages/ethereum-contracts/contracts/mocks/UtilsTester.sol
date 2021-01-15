// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { Int96SafeMath } from "../utils/Int96SafeMath.sol";
import { UInt128SafeMath } from "../utils/UInt128SafeMath.sol";


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

    function testInt96SafeMathMul(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.mul(a, b, "testInt96SafeMathMul overflow");
    }

    function testInt96SafeMathAdd(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.add(a, b, "testInt96SafeMathAdd overflow");
    }

    function testInt96SafeMathSub(int96 a, int96 b) external pure returns (int96 c) {
        return Int96SafeMath.sub(a, b, "testInt96SafeMathSub overflow");
    }

    /// UInt128SafeMath

    function getUint128Max() external pure returns (uint128) {
        return type(uint128).max;
    }

    function testUInt128SafeMathAdd(uint128 a, uint128 b) external pure returns (uint128 c) {
        return UInt128SafeMath.add(a, b, "testUInt128SafeMathAdd overflow");
    }

    function testInt128SafeMathSub(uint128 a, uint128 b) external pure returns (uint128 c) {
        return UInt128SafeMath.sub(a, b, "testInt96SafeMathSub overflow");
    }

}
