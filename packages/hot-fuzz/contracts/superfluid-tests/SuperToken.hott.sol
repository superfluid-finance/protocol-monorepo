// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";


abstract contract SuperTokenHotFuzzMixin is HotFuzzBase {
    function upgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getOneTester(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.upgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b1 - b2);
        assert(b1 - b2 == a2 - a1);
        expectedTotalSupply += amount;
    }

    function downgrade(uint8 a, uint64 amount) public {
        require(amount > 0);
        SuperfluidTester tester = getOneTester(a);

        int256 a1 = superTokenBalanceOfNow(address(tester));
        require(a1 >= int256(uint256(amount)));
        int256 b1 = int256(token.balanceOf(address(tester)));
        tester.downgradeSuperToken(amount);
        int256 a2 = superTokenBalanceOfNow(address(tester));
        int256 b2 = int256(token.balanceOf(address(tester)));
        assert(int256(uint256(amount)) == b2 - b1);
        assert(b2 - b1 == a1 - a2);
        expectedTotalSupply -= amount;
    }
}

contract SuperTokenHotFuzz is SuperTokenHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        initTesters();
    }
}
