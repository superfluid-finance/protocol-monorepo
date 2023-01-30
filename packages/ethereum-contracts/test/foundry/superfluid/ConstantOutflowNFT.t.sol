// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {
    IERC165Upgradeable,
    IERC721Upgradeable,
    IERC721MetadataUpgradeable
} from "@openzeppelin/contracts-upgradeable/token/ERC721/extensions/IERC721MetadataUpgradeable.sol";

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    CFAv1NFTBase,
    ConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";

import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { CFAv1BaseTest, ConstantOutflowNFTMock } from "./CFAv1NFTBase.t.sol";

contract ConstantOutflowNFTTest is CFAv1BaseTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_RevertIf_ContractAlreadyInitialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantOutflowNFTProxy.initialize(
            superToken,
            string.concat("henlo", OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function testFuzz_RevertIf_OwnerOfForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.ownerOf(_tokenId);
    }

    function testFuzz_RevertIf_GetApprovedForNonExistentToken(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.getApproved(_tokenId);
    }

    function testFuzz_RevertIf_NotInflowNFTCallingInflowTransferMint(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_ONLY_CONSTANT_INFLOW.selector
        );
        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.inflowTransferMint(
            _flowSender,
            _flowReceiver,
            nftId
        );
    }

    function testFuzz_RevertIf_NotInflowNFTCallingInflowTransferBurn(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_ONLY_CONSTANT_INFLOW.selector
        );
        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.inflowTransferBurn(nftId);
    }

    function testFuzz_RevertIf_InternalBurnNonExistentToken(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.mockBurn(_tokenId);
    }

    function testFuzz_RevertIf_InternalMintToZeroAddress(
        address _flowReceiver
    ) public {
        uint256 nftId = helper_getNFTId(address(0), _flowReceiver);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_ZERO_ADDRESS.selector
        );
        constantOutflowNFTProxy.mockMint(address(0), _flowReceiver, nftId);
    }

    function testFuzz_RevertIf_InternalMintTokenThatExists(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_TOKEN_ALREADY_EXISTS.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
    }

    function testFuzz_RevertIf_InternalMintSameToAndFlowReceiver(
        address _flowSender
    ) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = helper_getNFTId(_flowSender, _flowSender);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowSender, nftId);
    }

    function testFuzz_RevertIf_SetApprovalForAllOperatorApproveToCaller(
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
        vm.prank(_flowSender);
        constantOutflowNFTProxy.setApprovalForAll(_flowSender, true);
    }

    function testFuzz_RevertIf_ApproveToCurrentOwner(
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
        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_flowSender, nftId);
    }

    function testFuzz_RevertIf_ApproveAsNonOwner(
        address _flowSender,
        address _flowReceiver,
        address _approver,
        address _approvedAccount
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );
        /// @dev _flowSender is owner of outflow NFT
        vm.assume(_approver != _flowSender);
        vm.assume(_approvedAccount != _flowSender);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_approver);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_SupportsInterface() public {
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC165Upgradeable).interfaceId
            ),
            true
        );
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC721Upgradeable).interfaceId
            ),
            true
        );
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC721MetadataUpgradeable).interfaceId
            ),
            true
        );
    }

    function test_Passing_ConstantOutflowNFTDeploymentAndStateInitialization()
        public
    {
        string memory symbol = superToken.symbol();

        assertEq(
            constantOutflowNFTProxy.name(),
            string.concat(symbol, OUTFLOW_NFT_NAME_TEMPLATE)
        );
        assertEq(
            constantOutflowNFTProxy.symbol(),
            string.concat(symbol, OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function testFuzz_Passing_InternalMintToken(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);

        assert_Event_Transfer(address(0), _flowSender, nftId);

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);
    }

    function testFuzz_Passing_InternalBurnToken(
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

        constantOutflowNFTProxy.mockBurn(nftId);
        assert_FlowDataState_IsEmpty(nftId);
    }

    function testFuzz_Passing_Approve(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        assume_SenderNotEqReceiverAndNeitherAreZeroAddress(
            _flowSender,
            _flowReceiver
        );
        vm.assume(_flowSender != _approvedAccount);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        assert_Event_Approval(_flowSender, _approvedAccount, nftId);

        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);

        assert_Approval_IsExpected(
            constantOutflowNFTProxy,
            nftId,
            _approvedAccount
        );
    }

    function testFuzz_Passing_SetApprovalForAll(
        address _tokenOwner,
        address _operator,
        bool _approved
    ) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        assert_Event_ApprovalForAll(_tokenOwner, _operator, _approved);

        vm.prank(_tokenOwner);
        constantOutflowNFTProxy.setApprovalForAll(_operator, _approved);

        assert_OperatorApproval_IsExpected(
            constantOutflowNFTProxy,
            _tokenOwner,
            _operator,
            _approved
        );
    }
}
