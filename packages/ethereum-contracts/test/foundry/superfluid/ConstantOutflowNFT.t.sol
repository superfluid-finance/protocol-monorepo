// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.21;

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
import { SuperToken, SuperTokenMock } from "../../../contracts/mocks/SuperTokenMock.sol";
import { ConstantOutflowNFTMock, NoNFTSuperTokenMock } from "../../../contracts/mocks/CFAv1NFTMock.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { ISuperToken } from "../../../contracts/superfluid/SuperToken.sol";

contract ConstantOutflowNFTTest is FlowNFTBaseTest {
    using Strings for uint256;
    using SuperTokenV1Library for ISuperToken;
    using SuperTokenV1Library for SuperTokenMock;

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testRevertIfContractAlreadyInitialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantOutflowNFTProxy.initialize(
            string.concat("henlo", OUTFLOW_NFT_NAME_TEMPLATE), string.concat("goodbye", OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function testRevertIfOwnerOfForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.ownerOf(_tokenId);
    }

    function testRevertIfGetApprovedForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.getApproved(_tokenId);
    }

    function testRevertIfSetApprovalForAllOperatorApproveToCaller(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);
        vm.prank(_flowSender);
        constantOutflowNFTProxy.setApprovalForAll(_flowSender, true);
    }

    function testRevertIfApproveToCurrentOwner(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);
        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_flowSender, nftId);
    }

    function testRevertIfApproveAsNonOwner(
        address _flowSender,
        address _flowReceiver,
        address _approver,
        address _approvedAccount
    ) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        /// @dev _flowSender is owner of outflow NFT
        vm.assume(_approver != _flowSender);
        vm.assume(_approvedAccount != _flowSender);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_approver);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);
    }

    function testRevertIfInternalMintToZeroAddress(address _flowReceiver) public {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), address(0), _flowReceiver);
        vm.expectRevert();
        constantOutflowNFTProxy.mockMint(address(superTokenMock), address(0), _flowReceiver, nftId);
    }

    function testRevertIfInternalMintTokenThatExists(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert();
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
    }

    function testRevertIfInternalMintSameToAndFlowReceiver(address _flowSender) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowSender);
        vm.expectRevert();
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowSender, nftId);
    }

    function testRevertIfYouTryToTransferOutflowNFT(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFTProxy), address(0), _flowSender, nftId);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.safeTransferFrom(_flowSender, _flowReceiver, nftId);

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.safeTransferFrom(_flowSender, _flowReceiver, nftId, "0x");
    }

    function testRevertIfYouAreNotTheOwnerAndTryToTransferOutflowNFT(address _flowSender, address _flowReceiver)
        public
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFTProxy), address(0), _flowSender, nftId);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.safeTransferFrom(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.safeTransferFrom(_flowSender, _flowReceiver, nftId, "0x");
    }

    function testRevertIfOnCreateIsNotCalledByCFAv1(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        vm.prank(caller);
        constantOutflowNFTProxy.onCreate(superToken, address(1), address(2));
    }

    function testRevertIfOnUpdateIsNotCalledByCFAv1(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        vm.prank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFTProxy.onUpdate(superToken, address(1), address(2));
    }

    function testRevertIfOnDeleteIsNotCalledByCFAv1(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(sf.cfa));
        vm.prank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFTProxy.onDelete(superToken, address(1), address(2));
    }

    function testRevertGetNoFlowTokenURI() public {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), alice, bob);
        vm.expectRevert();
        constantOutflowNFTProxy.tokenURI(nftId);
        vm.expectRevert();
        constantInflowNFTProxy.tokenURI(nftId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testContractSupportsExpectedInterfaces() public {
        assertEq(constantOutflowNFTProxy.supportsInterface(type(IERC165).interfaceId), true);
        assertEq(constantOutflowNFTProxy.supportsInterface(type(IERC721).interfaceId), true);
        assertEq(constantOutflowNFTProxy.supportsInterface(type(IERC721Metadata).interfaceId), true);
    }

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            constantOutflowNFTProxy.proxiableUUID(),
            keccak256("org.superfluid-finance.contracts.ConstantOutflowNFT.implementation")
        );
    }

    function testNFTBalanceOfIsAlwaysOne(address _owner) public {
        assertEq(constantInflowNFTProxy.balanceOf(_owner), 1);
    }

    function testConstantOutflowNFTIsProperlyInitialized() public {
        assertEq(constantOutflowNFTProxy.name(), OUTFLOW_NFT_NAME_TEMPLATE);
        assertEq(constantOutflowNFTProxy.symbol(), OUTFLOW_NFT_SYMBOL_TEMPLATE);
    }

    function testInternalMintToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFTProxy), address(0), _flowSender, nftId);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );
    }

    function testInternalBurnToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventTransfer(address(constantOutflowNFTProxy), _flowSender, address(0), nftId);

        constantOutflowNFTProxy.mockBurn(nftId);
        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testApprove(address _flowSender, address _flowReceiver, address _approvedAccount)
        public
        returns (uint256 nftId)
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        vm.assume(_flowSender != _approvedAccount);

        nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventApproval(address(constantOutflowNFTProxy), _flowSender, _approvedAccount, nftId);

        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);

        _assertApprovalIsExpected(constantOutflowNFTProxy, nftId, _approvedAccount);
    }

    function testApproveThenBurn(address _flowSender, address _flowReceiver, address _approvedAccount) public {
        uint256 nftId = testApprove(_flowSender, _flowReceiver, _approvedAccount);
        constantOutflowNFTProxy.mockBurn(nftId);

        assertEq(constantOutflowNFTProxy.mockGetApproved(nftId), address(0));
    }

    function testSetApprovalForAll(address _tokenOwner, address _operator, bool _approved) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        _assertEventApprovalForAll(address(constantOutflowNFTProxy), _tokenOwner, _operator, _approved);

        vm.prank(_tokenOwner);
        constantOutflowNFTProxy.setApprovalForAll(_operator, _approved);

        _assertOperatorApprovalIsExpected(constantOutflowNFTProxy, _tokenOwner, _operator, _approved);
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
        _assertEventMetadataUpdate(address(constantOutflowNFTProxy), nftId);
        _assertEventMetadataUpdate(address(constantInflowNFTProxy), nftId);

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

        _assertEventTransfer(address(constantInflowNFTProxy), flowReceiver, address(0), nftId);

        _assertEventTransfer(address(constantOutflowNFTProxy), flowSender, address(0), nftId);

        vm.prank(flowSender);
        superTokenMock.deleteFlow(flowSender, flowReceiver);

        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testTokenURIIsExpected() public {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        _helperCreateFlowAndAssertNFTInvariants(flowSender, flowReceiver, flowRate);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), flowSender, flowReceiver);

        assertEq(
            constantOutflowNFTProxy.tokenURI(nftId),
            string(
                abi.encodePacked(
                    "https://nft.superfluid.finance/cfa/v2/getmeta?flowRate=",
                    uint256(uint96(flowRate)).toString(),
                    "&outgoing=true",
                    "&token_address=",
                    Strings.toHexString(uint256(uint160(address(superTokenMock))), 20),
                    "&chain_id=",
                    block.chainid.toString(),
                    "&token_symbol=",
                    superTokenMock.symbol(),
                    "&sender=",
                    Strings.toHexString(uint256(uint160(flowSender)), 20),
                    "&receiver=",
                    Strings.toHexString(uint256(uint160(flowReceiver)), 20),
                    "&token_decimals=",
                    uint256(superTokenMock.decimals()).toString(),
                    "&start_date=1" // timestamp shifts 1
                )
            )
        );
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
