// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.23;

import { FoundrySuperfluidTester, ISuperToken, SuperTokenV1Library, ISuperfluidPool }
    from "../FoundrySuperfluidTester.sol";

/*
* Note: since libs are used by contracts, not EOAs, do NOT try to use
* vm.prank() in tests. That will lead to unexpected outcomes.
* Instead, let the Test contract itself be the mock sender.
*/
contract SuperTokenV1LibraryTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for ISuperToken;

    constructor() FoundrySuperfluidTester(3) {
    }

    function setUp() public override {
        super.setUp();

        // fund this Test contract with SuperTokens
        vm.startPrank(alice);
        superToken.transfer(address(this), 10e18);
        vm.stopPrank();
    }

    // direct use of the agreement for assertions
    function _getCFAFlowRate(address sender, address receiver) public view returns (int96 flowRate) {
        (,flowRate,,) = sf.cfa.getFlow(superToken, sender, receiver);
    }

    // Note: this is without adjustmentFR
    function _getGDAFlowRate(address sender, ISuperfluidPool pool) public view returns (int96 flowRate) {
        return sf.gda.getFlowRate(superToken, sender, pool);
    }

    function testSetFlowrate() external {
        int96 fr1 = 1e9;

        // initial createFlow
        superToken.setFlowrate(bob, fr1);
        assertEq(_getCFAFlowRate(address(this), bob), fr1, "createFlow unexpected result");

        // double it -> updateFlow
        superToken.setFlowrate(bob, fr1 * 2);
        assertEq(_getCFAFlowRate(address(this), bob), fr1 * 2, "updateFlow unexpected result");

        // set to 0 -> deleteFlow
       superToken.setFlowrate(bob, 0);
       assertEq(_getCFAFlowRate(address(this), bob), 0, "deleteFlow unexpected result");
    }

    function testSetFlowrateFrom() external {
        int96 fr1 = 1e9;

        // alice allows this Test contract to operate CFA flows on her behalf
        vm.startPrank(alice);
        sf.host.callAgreement(
            sf.cfa,
            abi.encodeCall(sf.cfa.authorizeFlowOperatorWithFullControl, (superToken, address(this), new bytes(0))),
            new bytes(0) // userData
        );
        vm.stopPrank();

        // initial createFlow
        superToken.setFlowrateFrom(alice, bob, fr1);
        assertEq(_getCFAFlowRate(alice, bob), fr1, "createFlow unexpected result");

        // double it -> updateFlow
        superToken.setFlowrateFrom(alice, bob, fr1 * 2);
        assertEq(_getCFAFlowRate(alice, bob), fr1 * 2, "updateFlow unexpected result");

        // set to 0 -> deleteFlow
       superToken.setFlowrateFrom(alice, bob, 0);
       assertEq(_getCFAFlowRate(alice, bob), 0, "deleteFlow unexpected result");
    }

    function testFlowXToAccount() external {
        int96 fr1 = 1e9;

        superToken.flowX(bob, fr1);
        assertEq(_getCFAFlowRate(address(this), bob), fr1, "createFlow unexpected result");

        // double it -> updateFlow
        superToken.flowX(bob, fr1 * 2);
        assertEq(_getCFAFlowRate(address(this), bob), fr1 * 2, "updateFlow unexpected result");

        // set to 0 -> deleteFlow
       superToken.flowX(bob, 0);
       assertEq(_getCFAFlowRate(address(this), bob), 0, "deleteFlow unexpected result");
    }

    function testFlowXToPool() external {
        int96 fr1 = 1e9;

        ISuperfluidPool pool = superToken.createPool();
        pool.updateMemberUnits(bob, 1);

        superToken.flowX(address(pool), fr1);
        assertEq(_getGDAFlowRate(address(this), pool), fr1, "distrbuteFlow (new) unexpected result");

        // double it -> updateFlow
        superToken.flowX(address(pool), fr1 * 2);
        assertEq(_getGDAFlowRate(address(this), pool), fr1 * 2, "distrbuteFlow (update) unexpected result");

        // set to 0 -> deleteFlow
       superToken.flowX(address(pool), 0);
       assertEq(_getGDAFlowRate(address(this), pool), 0, "distrbuteFlow (delete) unexpected result");
    }

    function testTransferXToAccount() external {
        uint256 amount = 1e18;

        uint256 bobBalBefore = superToken.balanceOf(bob);
        superToken.transferX(bob, amount);
        assertEq(superToken.balanceOf(bob) - bobBalBefore, amount, "transfer unexpected result");
    }

    function testTransferXToPool() external {
        uint256 amount = 1e18;

        uint256 bobBalBefore = superToken.balanceOf(bob);
        ISuperfluidPool pool = superToken.createPool();
        pool.updateMemberUnits(bob, 1);

        superToken.transferX(address(pool), amount);
        pool.claimAll(bob);
        assertEq(superToken.balanceOf(bob) - bobBalBefore, amount, "distribute unexpected result");
    }
}