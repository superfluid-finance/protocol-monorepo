// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC165, IERC721, IERC721Metadata } from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { ConstantOutflowNFT } from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { IFlowNFTBase } from "../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import { FlowNFTBase, ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { FlowNFTBaseTest } from "./FlowNFTBase.t.sol";

contract ConstantInflowNFTTest is FlowNFTBaseTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testRevertIfMintIsNotCalledByOutflowNFT(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(constantOutflowNFT));
        vm.expectRevert(IConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector);
        constantInflowNFT.mint(address(0), 69);
    }

    function testRevertIfBurnIsNotCalledByOutflowNFT(address caller) public {
        _assumeCallerIsNotOtherAddress(caller, address(constantOutflowNFT));
        vm.expectRevert(IConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector);
        constantInflowNFT.burn(69);
    }

    function testRevertIfYouTryToTransferInflowNFT(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);

        vm.startPrank(_flowReceiver);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFT.transferFrom(_flowReceiver, _flowSender, nftId);
        vm.stopPrank();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/

    function testProxiableUUIDIsExpectedValue() public {
        assertEq(
            constantInflowNFT.proxiableUUID(),
            keccak256("org.superfluid-finance.contracts.ConstantInflowNFT.implementation")
        );
    }

    function testConstantInflowNFTIsProperlyInitialized() public {
        assertEq(constantInflowNFT.name(), INFLOW_NFT_NAME_TEMPLATE);
        assertEq(constantInflowNFT.symbol(), INFLOW_NFT_SYMBOL_TEMPLATE);
    }

    function testFlowDataByTokenIdMint(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        IFlowNFTBase.FlowNFTData memory flowData = constantInflowNFT.flowDataByTokenId(nftId);
        assertEq(flowData.flowSender, _flowSender);
        assertEq(flowData.flowReceiver, _flowReceiver);
    }

    function testInternalMintToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantInflowNFT), address(0), _flowReceiver, nftId);

        constantInflowNFT.mockMint(_flowReceiver, nftId);

        _assertNFTFlowDataStateIsEmpty(nftId);
    }

    function testInternalBurnToken(address _flowSender, address _flowReceiver) public {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);

        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventTransfer(address(constantInflowNFT), _flowReceiver, address(0), nftId);

        constantInflowNFT.mockBurn(nftId);

        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );
    }

    function testApprove(address _flowSender, address _flowReceiver, address _approvedAccount)
        public
        override
        returns (uint256 nftId)
    {
        _assumeSenderNEQReceiverAndNeitherAreZeroAddress(_flowSender, _flowReceiver);
        vm.assume(_flowReceiver != _approvedAccount);

        nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);
        constantOutflowNFT.mockMint(address(superTokenMock), _flowSender, _flowReceiver, nftId);
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        _assertEventApproval(address(constantInflowNFT), _flowReceiver, _approvedAccount, nftId);

        vm.startPrank(_flowReceiver);
        constantInflowNFT.approve(_approvedAccount, nftId);
        vm.stopPrank();

        _assertApprovalIsExpected(constantInflowNFT, nftId, _approvedAccount);
    }

    function testApproveThenBurn(address _flowSender, address _flowReceiver, address _approvedAccount) public {
        uint256 nftId = testApprove(_flowSender, _flowReceiver, _approvedAccount);
        constantInflowNFT.mockBurn(nftId);

        assertEq(constantInflowNFT.mockGetApproved(nftId), address(0));
    }

    function testSetApprovalForAll(address _tokenOwner, address _operator, bool _approved) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        vm.startPrank(_tokenOwner);
        _assertEventApprovalForAll(address(constantInflowNFT), _tokenOwner, _operator, _approved);
        constantInflowNFT.setApprovalForAll(_operator, _approved);
        vm.stopPrank();

        _assertOperatorApprovalIsExpected(constantInflowNFT, _tokenOwner, _operator, _approved);
    }
}
