// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.23;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { IERC165, IERC721, IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    FlowNFTBase, ConstantOutflowNFT, IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FoundrySuperfluidTester, SuperTokenV1Library } from "../FoundrySuperfluidTester.sol";
import { IFlowNFTBase } from "../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import { FlowNFTBaseTest } from "./FlowNFTBase.t.sol";
import { SuperToken, SuperTokenMock } from "./SuperTokenMock.t.sol";
import { ConstantOutflowNFTMock } from "./CFAv1NFTMock.t.sol";
import { NoNFTSuperTokenMock } from "./SuperTokenMock.t.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";


library StringExtra {
    function concat(string memory a, string memory b) internal pure returns (string memory) {
        return string(abi.encodePacked(a, b));
    }
}
using StringExtra for string;

contract ConstantOutflowNFTTest is FlowNFTBaseTest {
    using Strings for uint256;
    using SuperTokenV1Library for ISuperToken;
    using SuperTokenV1Library for SuperTokenMock;

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testRevertIfInternalMintToZeroAddress(address _flowReceiver) public {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), address(0), _flowReceiver);
        vm.expectRevert();
        constantOutflowNFT.mockMint(address(superTokenMock), address(0), _flowReceiver, nftId);
    }

    // test disabled -we do now explicitly allow this in order to not revert
    // if previous onDelete hooks failed to execute
    function noTestRevertIfInternalMintTokenThatExists(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert();
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
    }

    function testRevertIfInternalMintSameToAndFlowReceiver(address _flowSender) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowSender);
        vm.expectRevert();
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowSender, nftId);
    }

    function testRevertIfOnCreateIsNotCalledByFlowAgreement(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        _assumeCallerIsNotOtherAddress(caller, address(sf.gda));

        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        vm.prank(caller);
        constantOutflowNFT.onCreate(superToken, address(1), address(2));
    }

    function testRevertIfOnUpdateIsNotCalledByFlowAgreement(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        _assumeCallerIsNotOtherAddress(caller, address(sf.gda));

        vm.startPrank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFT.onUpdate(superToken, address(1), address(2));
        vm.stopPrank();
    }

    function testRevertIfOnDeleteIsNotCalledByFlowAgreement(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        _assumeCallerIsNotOtherAddress(caller, address(sf.gda));
        vm.prank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFT.onDelete(superToken, address(1), address(2));
    }

    function testRevertIfGetNoFlowTokenURI() public {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), alice, bob);
        vm.expectRevert();
        constantOutflowNFT.tokenURI(nftId);
        vm.expectRevert();
        constantInflowNFT.tokenURI(nftId);
    }

    function testRevertIfYouTryToTransferOutflowNFT(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        vm.startPrank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFT.transferFrom(_flowSender, _flowReceiver, nftId);
        vm.stopPrank();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testProxiableUUIDIsExpectedValue() public view {
        assertEq(
            constantOutflowNFT.proxiableUUID(),
            keccak256("org.superfluid-finance.contracts.ConstantOutflowNFT.implementation")
        );
    }

    function testConstantOutflowNFTIsProperlyInitialized() public view {
        assertEq(constantOutflowNFT.name(), OUTFLOW_NFT_NAME_TEMPLATE);
        assertEq(constantOutflowNFT.symbol(), OUTFLOW_NFT_SYMBOL_TEMPLATE);
    }

    function testInternalMintToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFT), address(0), _flowSender, nftId);

        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );
    }

    function testInternalBurnToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventTransfer(address(constantOutflowNFT), _flowSender, address(0), nftId);

        constantOutflowNFT.mockBurn(nftId);
        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testApprove(address _flowSender, address _flowReceiver, address _approvedAccount)
        public
        override
        returns (uint256 nftId)
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        vm.assume(_flowSender != _approvedAccount);

        nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        _assertEventApproval(address(constantOutflowNFT), _flowSender, _approvedAccount, nftId);

        vm.startPrank(_flowSender);
        constantOutflowNFT.approve(_approvedAccount, nftId);
        vm.stopPrank();

        _assertApprovalIsExpected(constantOutflowNFT, nftId, _approvedAccount);
    }

    function testApproveThenBurn(address _flowSender, address _flowReceiver, address _approvedAccount) public {
        uint256 nftId = testApprove(_flowSender, _flowReceiver, _approvedAccount);
        constantOutflowNFT.mockBurn(nftId);

        assertEq(constantOutflowNFT.mockGetApproved(nftId), address(0));
    }

    function testSetApprovalForAll(address _tokenOwner, address _operator, bool _approved) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);


        vm.startPrank(_tokenOwner);
        _assertEventApprovalForAll(address(constantOutflowNFT), _tokenOwner, _operator, _approved);
        constantOutflowNFT.setApprovalForAll(_operator, _approved);
        vm.stopPrank();

        _assertOperatorApprovalIsExpected(constantOutflowNFT, _tokenOwner, _operator, _approved);
    }

    function testCreateFlowMintsOutflowAndInflowNFTsAndEmitsTransferEvents() public {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        _helperCreateFlowAndAssertNFTInvariants(flowSender, flowReceiver, flowRate);
    }

    function testUpdateFlowDoesNotImpactStorageAndEmitsMetadataUpdateEvents() public {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        _helperCreateFlowAndAssertNFTInvariants(flowSender, flowReceiver, flowRate);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), flowSender, flowReceiver);
        _assertEventMetadataUpdate(address(constantOutflowNFT), nftId);
        _assertEventMetadataUpdate(address(constantInflowNFT), nftId);

        vm.prank(flowSender);
        superTokenMock.updateFlow(flowReceiver, flowRate + 333);

        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), flowSender, uint32(block.timestamp), flowReceiver
        );
    }

    function testDeleteFlowClearsStorageAndEmitsTransferEvents() public {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        _helperCreateFlowAndAssertNFTInvariants(flowSender, flowReceiver, flowRate);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), flowSender, flowReceiver);

        _assertEventTransfer(address(constantInflowNFT), flowReceiver, address(0), nftId);

        _assertEventTransfer(address(constantOutflowNFT), flowSender, address(0), nftId);

        vm.prank(flowSender);
        superTokenMock.deleteFlow(flowSender, flowReceiver);

        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testTokenURIIsExpected(uint32 startDate, uint32 flowRate) public {
        vm.assume(flowRate > 0);
        address flowSender = alice;
        address flowReceiver = bob;

        vm.warp(startDate);
        _helperCreateFlowAndAssertNFTInvariants(flowSender, flowReceiver, int96(int256(uint256(flowRate))));

        string memory tokenURI;
        {
            uint256 nftId = _helperGetNFTID(address(superTokenMock), flowSender, flowReceiver);
            tokenURI = constantOutflowNFT.tokenURI(nftId);
        }

        string memory expectedTokenURI;
        {
            expectedTokenURI = string("https://nft.superfluid.finance/cfa/v2/getmeta?flowRate=")
                    .concat(uint256(flowRate).toString());
            expectedTokenURI = expectedTokenURI.concat("&outgoing=true");
            expectedTokenURI = expectedTokenURI.concat("&token_address=")
                     .concat(Strings.toHexString(uint256(uint160(address(superTokenMock))), 20));
            expectedTokenURI = expectedTokenURI.concat("&chain_id=")
                     .concat(block.chainid.toString());
            expectedTokenURI = expectedTokenURI.concat("&token_symbol=")
                    .concat(superTokenMock.symbol());
            expectedTokenURI = expectedTokenURI.concat("&sender=")
                     .concat(Strings.toHexString(uint256(uint160(flowSender)), 20));
            expectedTokenURI = expectedTokenURI.concat("&receiver=")
                     .concat(Strings.toHexString(uint256(uint160(flowReceiver)), 20));
            expectedTokenURI = expectedTokenURI.concat("&token_decimals=")
                     .concat(uint256(superTokenMock.decimals()).toString());
            expectedTokenURI = expectedTokenURI.concat("&start_date=")
                     .concat(uint256(startDate).toString());
        }

        assertEq(tokenURI, expectedTokenURI);
    }

    function testCreateUpdateDeleteFlowNoNFTToken() public {
        uint256 initialAmount = 10000 ether;
        TestToken testToken = new TestToken("Test", "TS", 18, initialAmount);
        NoNFTSuperTokenMock noNFTSuperTokenMock = new NoNFTSuperTokenMock(
            sf.host
        );
        noNFTSuperTokenMock.initialize(testToken, 18, "Super Test", "TSx");
        vm.startPrank(alice);
        testToken.mint(alice, initialAmount);
        testToken.approve(address(noNFTSuperTokenMock), initialAmount);
        noNFTSuperTokenMock.upgrade(initialAmount);
        ISuperToken(address(noNFTSuperTokenMock)).createFlow(bob, 100);
        (, int96 flowRate,,) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 100);
        ISuperToken(address(noNFTSuperTokenMock)).updateFlow(bob, 150);
        (, flowRate,,) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 150);
        ISuperToken(address(noNFTSuperTokenMock)).updateFlow(bob, 90);
        (, flowRate,,) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 90);
        ISuperToken(address(noNFTSuperTokenMock)).deleteFlow(alice, bob);
        (, flowRate,,) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 0);
        vm.stopPrank();
    }
}
