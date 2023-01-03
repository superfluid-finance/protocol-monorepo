// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    ISuperfluid,
    ISuperfluidToken
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    CFAOutflowNFT
} from "@superfluid-finance/ethereum-contracts/contracts/superfluid/CFAOutflowNFT.sol";
import {
    CFAInflowNFT
} from "@superfluid-finance/ethereum-contracts/contracts/superfluid/CFAInflowNFT.sol";
import {
    SuperTokenFactoryBase
} from "@superfluid-finance/ethereum-contracts/contracts/superfluid/SuperTokenFactory.sol";
import "../FoundrySuperfluidTester.sol";

contract SuperTokenCFAV1Anvil is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    struct InitializeData {
        address underlyingToken;
        address superToken;
    }

    constructor() FoundrySuperfluidTester(3) {}

    function testTokenCreateFlowFailIfNotCanonical(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.startPrank(alice);
        vm.expectRevert(ISuperfluid.HOST_UNTRUSTED_SUPER_TOKEN.selector);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();
    }

    function _initializeCanonicalWrapperSuperTokens() internal {
        vm.startPrank(address(sfDeployer));
        SuperTokenFactoryBase.InitializeData[]
            memory tokens = new SuperTokenFactoryBase.InitializeData[](1);
        SuperTokenFactoryBase.InitializeData
            memory tokenData = SuperTokenFactoryBase.InitializeData({
                underlyingToken: superToken.getUnderlyingToken(),
                superToken: address(superToken)
            });
        tokens[0] = tokenData;
        sf.superTokenFactory.initializeCanonicalWrapperSuperTokens(tokens);
        vm.stopPrank();
    }

    function testTokenCreateFlow(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        _initializeCanonicalWrapperSuperTokens();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate);

        assertTrue(checkAllInvariants());
    }

    function testTokenCreateFlowNFTCreation(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        _initializeCanonicalWrapperSuperTokens();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken._cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken._cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(alice), 1);
        assertEq(inflowNFT.balanceOf(bob), 1);

        assertTrue(checkAllInvariants());
    }
}
