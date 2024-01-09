// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";

abstract contract SuperTokenHotFuzzMixin is HotFuzzBase {
    function approve(uint8 a, uint8 b, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        testerA.approve(address(testerB), amount);
    }

    function increaseAllowance(uint8 a, uint8 b, uint256 addedValue) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        testerA.increaseAllowance(address(testerB), addedValue);
    }

    function decreaseAllowance(uint8 a, uint8 b, uint256 subtractedValue) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        testerA.decreaseAllowance(address(testerB), subtractedValue);
    }

    function transfer(uint8 a, uint8 b, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        testerA.transfer(address(testerB), amount);
    }

    function transferFrom(uint8 a, uint8 b, uint8 c, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        SuperfluidTester testerC = _getOneTester(c);
        testerA.transferFrom(address(testerB), address(testerC), amount);
    }

    function transferAll(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        testerA.transferAll(address(testerB));
    }

    function upgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = _getOneTester(a);

        int256 a1 = _superTokenBalanceOfNow(address(tester));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.upgradeSuperToken(amount);
        int256 a2 = _superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        expectedTotalSupply += amount;
    }

    function downgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = _getOneTester(a);

        int256 a1 = _superTokenBalanceOfNow(address(tester));
        require(a1 >= int256(uint256(amount)));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.downgradeSuperToken(amount);
        int256 a2 = _superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        expectedTotalSupply -= amount;
    }
}

contract SuperTokenHotFuzz is SuperTokenHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        _initTesters();
    }
}
