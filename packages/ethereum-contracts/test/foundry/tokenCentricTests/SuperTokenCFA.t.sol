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

    function testTokenCreateFlowNonCanonicalFail(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        vm.expectRevert(ISuperfluid.HOST_UNTRUSTED_SUPER_TOKEN.selector);

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
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

    function testTokenCreateFlowNFTMinted(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        _initializeCanonicalWrapperSuperTokens();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(alice), 1);
        assertEq(inflowNFT.balanceOf(bob), 1);

        uint256 nftId = uint256(keccak256(abi.encode(alice, bob)));
        assertEq(outflowNFT.ownerOf(nftId), alice);
        assertEq(inflowNFT.ownerOf(nftId), bob);

        assertTrue(checkAllInvariants());
    }

    function testTokenUpdateFlow(uint32 a) public {
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

        int96 updatedFlowRate = flowRate + 420;

        vm.startPrank(alice);
        superToken.updateFlow(bob, updatedFlowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), -updatedFlowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), updatedFlowRate);

        assertTrue(checkAllInvariants());
    }

    function testTokenUpdateFlowNFTUnchanged(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        _initializeCanonicalWrapperSuperTokens();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(alice), 1);
        assertEq(inflowNFT.balanceOf(bob), 1);

        uint256 nftId = uint256(keccak256(abi.encode(alice, bob)));
        assertEq(outflowNFT.ownerOf(nftId), alice);
        assertEq(inflowNFT.ownerOf(nftId), bob);

        int96 updatedFlowRate = flowRate + 420;

        vm.startPrank(alice);
        superToken.updateFlow(bob, updatedFlowRate);
        vm.stopPrank();

        assertEq(outflowNFT.balanceOf(alice), 1);
        assertEq(inflowNFT.balanceOf(bob), 1);

        assertEq(outflowNFT.ownerOf(nftId), alice);
        assertEq(inflowNFT.ownerOf(nftId), bob);

        assertTrue(checkAllInvariants());
    }

    function testTokenDeleteFlow(uint32 a) public {
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

        vm.startPrank(alice);
        superToken.deleteFlow(bob);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), 0);
        assertEq(sf.cfa.getNetFlow(superToken, bob), 0);

        assertTrue(checkAllInvariants());
    }

    function testTokenDeleteFlowNFTBurned(uint32 a) public {
        vm.assume(a > 0);
        vm.assume(a <= uint32(type(int32).max));
        int96 flowRate = int96(int32(a));

        _initializeCanonicalWrapperSuperTokens();

        vm.startPrank(alice);
        superToken.createFlow(bob, flowRate);
        vm.stopPrank();

        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(alice), 1);
        assertEq(inflowNFT.balanceOf(bob), 1);

        uint256 nftId = uint256(keccak256(abi.encode(alice, bob)));
        assertEq(outflowNFT.ownerOf(nftId), alice);
        assertEq(inflowNFT.ownerOf(nftId), bob);

        vm.startPrank(alice);
        superToken.deleteFlow(bob);
        vm.stopPrank();

        assertEq(outflowNFT.balanceOf(alice), 0);
        assertEq(inflowNFT.balanceOf(bob), 0);

        assertEq(outflowNFT.ownerOf(nftId), address(0));
        assertEq(inflowNFT.ownerOf(nftId), address(0));
        assertTrue(checkAllInvariants());
    }
}
