// SPDX-License-Identifier: MIT
pragma solidity 0.8.19;

import "@superfluid-finance/ethereum-contracts/test/foundry/FoundrySuperfluidTester.sol";
import "../src/SimpleSuperTokenUnwrapper.sol";

contract SimpleSuperTokenUnwrapperTester is FoundrySuperfluidTester {
    int96 private _TEST_FLOW_RATE = 486111111111111; // 42 ether / 1 days

    using SuperTokenV1Library for SuperToken;

    StreamingInvoiceTemplates private _ts;

    constructor() FoundrySuperfluidTester(10) {}

    function setUp() public override {
        FoundrySuperfluidTester.setUp();
        _ts = new StreamingInvoiceTemplates();
    }

    // Scenario:
    // - bob (payee) issues a payment request to alice (payer)
    // - carol is bob's assistant
    // - dan fat fingered and send some underlying tokens to the unwrapper
    function test_smoke_simple_super_token_unwrapper() external {
        uint t0 = block.timestamp;

        vm.startPrank(carol);
        SimpleSuperTokenUnwrapper t = _ts.createSimpleSuperTokenUnwrapper(alice, address(superToken), bob);
        vm.stopPrank();

        vm.startPrank(alice);
        superToken.createFlow(address(t), _TEST_FLOW_RATE);
        vm.stopPrank();

        {
            uint a1 = token.balanceOf(bob);

            uint t1 = t0 + 1 days;
            vm.warp(t1);

            vm.startPrank(carol);
            t.releaseAll();
            vm.stopPrank();

            uint a2 = token.balanceOf(bob);
            assertEq(a2 - a1, uint(uint96(_TEST_FLOW_RATE)) * 1 days, "e2");
        }

        {
            uint a1 = token.balanceOf(alice);
            vm.startPrank(dan);
            token.transfer(address(t), 42 ether);
            vm.stopPrank();
            vm.startPrank(carol);
            t.refundERC20(address(token));
            vm.stopPrank();
            uint a2 = token.balanceOf(alice);
            assertEq(a2 - a1, 42 ether, "e3");
        }
    }
}
