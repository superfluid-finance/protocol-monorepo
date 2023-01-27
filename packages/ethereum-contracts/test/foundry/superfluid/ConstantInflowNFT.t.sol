// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    IERC165Upgradeable,
    IERC721Upgradeable,
    IERC721MetadataUpgradeable
} from "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/IERC721MetadataUpgradeable.sol";

import {
    ConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    CFAv1NFTBase,
    ConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";

import { CFAv1BaseTest, ConstantOutflowNFTMock } from "./CFAv1NFTBase.t.sol";

contract ConstantInflowNFTTest is CFAv1BaseTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_RevertIf_ContractAlreadyInitialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantInflowNFTProxy.initialize(
            superToken,
            string.concat("henlo", INFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_RevertIf_OwnerOfForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.ownerOf(_tokenId);
    }

    function test_RevertIf_GetApprovedForNonExistentToken(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.getApproved(_tokenId);
    }

    function test_RevertIf_SetApprovalForAllOperatorApproveToCaller(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.setApprovalForAll(_flowReceiver, true);
    }

    function test_RevertIf_ApproveToCurrentOwner(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_flowReceiver, nftId);
    }

    function test_RevertIf_ApproveAsNonOwner(
        address _flowSender,
        address _flowReceiver,
        address _approver,
        address _approvedAccount
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );
        /// @dev _flowReceiver is owner of inflow NFT
        vm.assume(_approver != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_approver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_SupportsInterface() public {
        assertEq(
            constantInflowNFTProxy.supportsInterface(
                type(IERC165Upgradeable).interfaceId
            ),
            true
        );
        assertEq(
            constantInflowNFTProxy.supportsInterface(
                type(IERC721Upgradeable).interfaceId
            ),
            true
        );
        assertEq(
            constantInflowNFTProxy.supportsInterface(
                type(IERC721MetadataUpgradeable).interfaceId
            ),
            true
        );
    }

    function test_Passing_ConstantInflowNFTDeploymentAndStateInitialization()
        public
    {
        string memory symbol = superToken.symbol();

        assertEq(
            constantInflowNFTProxy.name(),
            string.concat(symbol, INFLOW_NFT_NAME_TEMPLATE)
        );
        assertEq(
            constantInflowNFTProxy.symbol(),
            string.concat(symbol, INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_Passing_FlowDataByTokenIdMint(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        CFAv1NFTBase.FlowData memory flowData = constantInflowNFTProxy
            .mockFlowDataByTokenId(nftId);
        assertEq(flowData.flowSender, _flowSender);
        assertEq(flowData.flowReceiver, _flowReceiver);
    }

    function test_Passing_InternalMintToken(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);

        assert_Event_Transfer(address(0), _flowReceiver, nftId);

        constantInflowNFTProxy.mockMint(_flowReceiver, nftId);

        assert_FlowDataState_IsEmpty(nftId);
    }

    function test_Passing_InternalBurnToken(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        assert_Event_Transfer(_flowReceiver, address(0), nftId);

        constantInflowNFTProxy.mockBurn(nftId);

        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);
    }

    function test_Passing_Approve(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );
        vm.assume(_flowReceiver != _approvedAccount);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        assert_Event_Approval(_flowReceiver, _approvedAccount, nftId);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);

        assert_Approval_IsExpected(
            constantInflowNFTProxy,
            nftId,
            _approvedAccount
        );
    }

    function test_Passing_SetApprovalForAll(
        address _tokenOwner,
        address _operator,
        bool _approved
    ) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        assert_Event_ApprovalForAll(_tokenOwner, _operator, _approved);

        vm.prank(_tokenOwner);
        constantInflowNFTProxy.setApprovalForAll(_operator, _approved);

        assert_OperatorApproval_IsExpected(
            constantInflowNFTProxy,
            _tokenOwner,
            _operator,
            _approved
        );
    }
}
