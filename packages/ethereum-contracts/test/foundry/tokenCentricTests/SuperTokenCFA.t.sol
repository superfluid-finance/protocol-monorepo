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

    constructor() FoundrySuperfluidTester(4) {}

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

    function _createFlowViaSuperTokenAndAssertProperties(
        uint32 _flowRate,
        address _sender,
        address _receiver
    ) internal returns (int96 flowRate) {
        vm.assume(_flowRate > 0);
        vm.assume(_flowRate <= uint32(type(int32).max));
        flowRate = int96(int32(_flowRate));

        vm.startPrank(_sender);
        superToken.createFlow(_receiver, flowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, _sender), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, _receiver), flowRate);

    }

    function _assertCreateFlowNFTInvariants(
        address _sender,
        address _receiver,
        uint256 _senderExpectedBalance,
        uint256 _receiverExpectedBalance
    ) internal returns (uint256 nftId) {
        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(_sender), _senderExpectedBalance);
        assertEq(inflowNFT.balanceOf(_receiver), _receiverExpectedBalance);

        nftId = uint256(keccak256(abi.encode(_sender, _receiver)));
        assertEq(outflowNFT.ownerOf(nftId), _sender);
        assertEq(inflowNFT.ownerOf(nftId), _receiver);
    }

    function _assertDeleteFlowNFTInvariants(
        address _sender,
        address _receiver,
        uint256 _senderExpectedBalance,
        uint256 _receiverExpectedBalance
    ) internal returns (uint256 nftId) {
        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        assertEq(outflowNFT.balanceOf(_sender), _senderExpectedBalance);
        assertEq(inflowNFT.balanceOf(_receiver), _receiverExpectedBalance);

        nftId = uint256(keccak256(abi.encode(_sender, _receiver)));
        assertEq(outflowNFT.ownerOf(nftId), address(0));
        assertEq(inflowNFT.ownerOf(nftId), address(0));
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
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        assertTrue(checkAllInvariants());
    }

    function testTokenCreateFlowNFTMinted(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);
        _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());
    }

    function testTokenUpdateFlow(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        int96 flowRate = _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        int96 updatedFlowRate = flowRate + 420;

        vm.startPrank(alice);
        superToken.updateFlow(bob, updatedFlowRate);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), -updatedFlowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), updatedFlowRate);

        assertTrue(checkAllInvariants());
    }

    function testTokenUpdateFlowNFTUnchanged(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        int96 flowRate = _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        int96 updatedFlowRate = flowRate + 420;

        vm.startPrank(alice);
        superToken.updateFlow(bob, updatedFlowRate);
        vm.stopPrank();

        _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());
    }

    function testTokenDeleteFlow(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        assertTrue(checkAllInvariants());

        vm.startPrank(alice);
        superToken.deleteFlow(bob);
        vm.stopPrank();

        assertEq(sf.cfa.getNetFlow(superToken, alice), 0);
        assertEq(sf.cfa.getNetFlow(superToken, bob), 0);

        assertTrue(checkAllInvariants());
    }

    function testTokenDeleteFlowNFTBurned(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        vm.startPrank(alice);
        superToken.deleteFlow(bob);
        vm.stopPrank();

        _assertDeleteFlowNFTInvariants(alice, bob, 0, 0);

        assertTrue(checkAllInvariants());
    }

    function testTransferInflowNFTFail(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());

        // transfer inflow NFT
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        vm.expectRevert(CFAInflowNFT.CFA_INFLOW_NFT_ONLY_OWNER_OR_APPROVED.selector);
        vm.startPrank(carol);
        inflowNFT.transferFrom(bob, carol, nftId);
        vm.stopPrank();
    }

    function testTransferInflowNFT(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        int96 flowRate = _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());

        // transfer inflow NFT
        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        vm.startPrank(bob);
        inflowNFT.transferFrom(bob, carol, nftId);
        vm.stopPrank();

        // check that the flow is from alice to bob is deleted
        // and a new flow is created from alice to carol
        assertEq(sf.cfa.getNetFlow(superToken, alice), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), 0);
        assertEq(sf.cfa.getNetFlow(superToken, carol), flowRate);

        // check that the NFTs are transferred
        _assertCreateFlowNFTInvariants(alice, carol, 1, 1);
        _assertDeleteFlowNFTInvariants(alice, bob, 1, 0);

        assertTrue(checkAllInvariants());
    }

    function testApproveAndTransferInflowNFT(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());


        CFAInflowNFT inflowNFT = CFAInflowNFT(superToken.cfaInflowNFT());
        // approve nft transfer
        vm.startPrank(bob);
        inflowNFT.approve(carol, nftId);
        vm.stopPrank();

        // transfer inflow NFT
        vm.startPrank(carol);
        inflowNFT.transferFrom(bob, carol, nftId);
        vm.stopPrank();
    }

    function testTransferOutflowNFTFail(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());

        // transfer outflow NFT
        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());
        vm.expectRevert(CFAOutflowNFT.CFA_OUTFLOW_NFT_ONLY_OWNER_OR_APPROVED.selector);
        vm.startPrank(dan);
        outflowNFT.transferFrom(alice, carol, nftId);
        vm.stopPrank();
    }

    function testTransferOutflowNFTAsNFTOwner(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();

        // alice creates a flow to bob, outflow nft alice->bob is created
        int96 flowRate = _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());

        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());

        // @note carol has to approve nft transfer?
        // might make more sense to just do via acl directly

        // alice wants to transfer the outflow nft to carol
        // alice must have acl permissions to do so from carol
        // carol provides acl permissions via nft
        vm.startPrank(carol);
        superToken.setFlowPermissions(alice, true, false, false, flowRate);
        vm.stopPrank();

        // alice transfers the nft to carol
        // this should delete the flow from alice to bob and create a new flow from carol to bob
        // transfer outflow NFT
        vm.startPrank(alice);
        outflowNFT.transferFrom(alice, carol, nftId);
        vm.stopPrank();

        // check that the flow is from alice to bob is deleted
        // and a new flow is created from carol to bob
        assertEq(sf.cfa.getNetFlow(superToken, alice), 0);
        assertEq(sf.cfa.getNetFlow(superToken, carol), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate);

        // check that the NFTs are transferred
        _assertCreateFlowNFTInvariants(carol, bob, 1, 1);
        _assertDeleteFlowNFTInvariants(alice, bob, 0, 1);

        assertTrue(checkAllInvariants());
    }

    function testTransferOutflowNFTWithOwnerApproval(uint32 a) public {
        _initializeCanonicalWrapperSuperTokens();
        // alice creates a flow to bob
        int96 flowRate = _createFlowViaSuperTokenAndAssertProperties(a, alice, bob);

        uint256 nftId = _assertCreateFlowNFTInvariants(alice, bob, 1, 1);

        assertTrue(checkAllInvariants());

        // carol transfers the outflow
        CFAOutflowNFT outflowNFT = CFAOutflowNFT(superToken.cfaOutflowNFT());

        // alice wants to transfer the outflow nft to carol
        // alice must have acl permissions to do so from carol
        // carol provides acl permissions via nft
        vm.startPrank(alice);
        outflowNFT.approve(carol, nftId);
        vm.stopPrank();

        // transfer outflow NFT
        vm.startPrank(carol);
        outflowNFT.transferFrom(alice, carol, nftId);
        vm.stopPrank();

        // check that the flow is from alice to bob is deleted
        // check that a flow from carol to bob is created
        assertEq(sf.cfa.getNetFlow(superToken, alice), 0);
        assertEq(sf.cfa.getNetFlow(superToken, carol), -flowRate);
        assertEq(sf.cfa.getNetFlow(superToken, bob), flowRate);
    }
}
