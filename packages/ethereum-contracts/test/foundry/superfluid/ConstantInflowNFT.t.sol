// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.20;

import { IERC165 } from "@openzeppelin/contracts/interfaces/IERC165.sol";
import { IERC721, IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { ConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { IFlowNFTBase } from "../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import { FlowNFTBase, ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FlowNFTBaseTest } from "./FlowNFTBase.t.sol";

contract ConstantInflowNFTTest is FlowNFTBaseTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testRevertIfContractAlreadyInitialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantInflowNFTProxy.initialize(
            string.concat("henlo", INFLOW_NFT_NAME_TEMPLATE), string.concat("goodbye", INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function testRevertIfOwnerOfCalledForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.ownerOf(_tokenId);
    }

    function testRevertIfGetApprovedCalledForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.getApproved(_tokenId);
    }

    function testRevertIfApproveToCallerWhenSetApprovalForAll(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.setApprovalForAll(_flowReceiver, true);
    }

    function testRevertIfApproveToCurrentOwner(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_flowReceiver, nftId);
    }

    function testRevertIfApproveAsNonOwner(
        address _flowSender,
        address _flowReceiver,
        address _approver,
        address _approvedAccount
    ) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        /// @dev _flowReceiver is owner of inflow NFT
        vm.assume(_approver != _flowReceiver);
        vm.assume(_approvedAccount != _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_approver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);
    }

    function testRevertIfYouTryToTransferInflowNFT(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFTProxy), address(0), _flowSender, nftId);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        vm.prank(_flowReceiver);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.transferFrom(_flowReceiver, _flowSender, nftId);

        vm.prank(_flowReceiver);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.safeTransferFrom(_flowReceiver, _flowSender, nftId);

        vm.prank(_flowReceiver);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.safeTransferFrom(_flowReceiver, _flowSender, nftId, "0x");
    }

    function testRevertIfYouAreNotTheOwnerAndTryToTransferInflowNFT(address _flowSender, address _flowReceiver)
        public
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowSender);
        constantInflowNFTProxy.transferFrom(_flowReceiver, _flowSender, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowSender);
        constantInflowNFTProxy.safeTransferFrom(_flowReceiver, _flowSender, nftId);

        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL.selector);
        vm.prank(_flowSender);
        constantInflowNFTProxy.safeTransferFrom(_flowReceiver, _flowSender, nftId, "0x");
    }

    function testRevertIfMintIsNotCalledByOutflowNFT(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(constantOutflowNFTProxy));
        vm.expectRevert(IConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector);
        constantInflowNFTProxy.mint(address(0), 69);
    }

    function testRevertIfBurnIsNotCalledByOutflowNFT(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(constantOutflowNFTProxy));
        vm.expectRevert(IConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector);
        constantInflowNFTProxy.burn(69);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testContractSupportsExpectedInterfaces() public {
        assertEq(constantInflowNFTProxy.supportsInterface(type(IERC165).interfaceId), true);
        assertEq(constantInflowNFTProxy.supportsInterface(type(IERC721).interfaceId), true);
        assertEq(constantInflowNFTProxy.supportsInterface(type(IERC721Metadata).interfaceId), true);
    }

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            constantInflowNFTProxy.proxiableUUID(),
            keccak256("org.superfluid-finance.contracts.ConstantInflowNFT.implementation")
        );
    }

    function testNFTBalanceOfIsAlwaysOne(address _owner) public {
        assertEq(constantInflowNFTProxy.balanceOf(_owner), 1);
    }

    function testConstantInflowNFTIsProperlyInitialized() public {
        assertEq(constantInflowNFTProxy.name(), INFLOW_NFT_NAME_TEMPLATE);
        assertEq(constantInflowNFTProxy.symbol(), INFLOW_NFT_SYMBOL_TEMPLATE);
    }

    function testFlowDataByTokenIdMint(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        IFlowNFTBase.FlowNFTData memory flowData = constantInflowNFTProxy.mockFlowNFTDataByTokenId(nftId);
        assertEq(flowData.flowSender, _flowSender);
        assertEq(flowData.flowReceiver, _flowReceiver);
    }

    function testInternalMintToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantInflowNFTProxy), address(0), _flowReceiver, nftId);

        constantInflowNFTProxy.mockMint(_flowReceiver, nftId);

        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testInternalBurnToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventTransfer(address(constantInflowNFTProxy), _flowReceiver, address(0), nftId);

        constantInflowNFTProxy.mockBurn(nftId);

        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );
    }

    function testApprove(address _flowSender, address _flowReceiver, address _approvedAccount)
        public
        returns (uint256 nftId)
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        vm.assume(_flowReceiver != _approvedAccount);

        nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventApproval(address(constantInflowNFTProxy), _flowReceiver, _approvedAccount, nftId);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);

        _assertApprovalIsExpected(constantInflowNFTProxy, nftId, _approvedAccount);
    }

    function testApproveThenBurn(address _flowSender, address _flowReceiver, address _approvedAccount) public {
        uint256 nftId = testApprove(_flowSender, _flowReceiver, _approvedAccount);
        constantInflowNFTProxy.mockBurn(nftId);

        assertEq(constantInflowNFTProxy.mockGetApproved(nftId), address(0));
    }

    function testSetApprovalForAll(address _tokenOwner, address _operator, bool _approved) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        vm.startPrank(_tokenOwner);
        _assertEventApprovalForAll(address(constantInflowNFTProxy), _tokenOwner, _operator, _approved);
        constantInflowNFTProxy.setApprovalForAll(_operator, _approved);
        vm.stopPrank();

        _assertOperatorApprovalIsExpected(constantInflowNFTProxy, _tokenOwner, _operator, _approved);
    }
}
