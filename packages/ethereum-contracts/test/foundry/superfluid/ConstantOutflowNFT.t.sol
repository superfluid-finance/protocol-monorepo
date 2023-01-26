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
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function assert_FlowDataState_IsExpected(
        uint256 _tokenId,
        address _expectedFlowSender,
        address _expectedFlowReceiver
    ) public {
        CFAv1NFTBase.FlowData memory flowData = constantOutflowNFTProxy
            .flowDataBySenderReceiver(_tokenId);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowSender, _expectedFlowSender);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowReceiver, _expectedFlowReceiver);

        // assert owner of outflow nft equal to expected flow sender
        assertEq(
            constantOutflowNFTProxy.mockOwnerOf(_tokenId),
            _expectedFlowSender
        );
    }

    function assert_FlowDataState_IsEmpty(uint256 _tokenId) public {
        assert_FlowDataState_IsExpected(_tokenId, address(0), address(0));
    }

    function assert_Approval_IsExpected(
        uint256 _tokenId,
        address _expectedApproved
    ) public {
        address approved = constantOutflowNFTProxy.getApproved(_tokenId);

        assertEq(approved, _expectedApproved);
    }

    function assert_OperatorApproval_IsExpected(
        address _expectedOwner,
        address _expectedOperator,
        bool _expectedOperatorApproval
    ) public {
        bool operatorApproval = constantOutflowNFTProxy.isApprovedForAll(
            _expectedOwner,
            _expectedOperator
        );

        assertEq(operatorApproval, _expectedOperatorApproval);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Cases
    //////////////////////////////////////////////////////////////////////////*/
    function test_RevertIf_ContractAlreadyInitialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantOutflowNFTProxy.initialize(
            superToken,
            string.concat("henlo", OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_RevertIf_OwnerOfForNonExistentToken(uint256 _tokenId) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.ownerOf(_tokenId);
    }

    function test_RevertIf_GetApprovedForNonExistentToken(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.getApproved(_tokenId);
    }

    function test_RevertIf_NotInflowNFTCallingInflowTransferMint(
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

    function test_RevertIf_NotInflowNFTCallingInflowTransferBurn(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_ONLY_CONSTANT_INFLOW.selector
        );
        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.inflowTransferBurn(nftId);
    }

    function test_RevertIf_InternalBurnNonExistentToken(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.mockBurn(_tokenId);
    }

    function test_RevertIf_InternalMintToZeroAddress(
        address _flowReceiver
    ) public {
        uint256 nftId = helper_getNFTId(address(0), _flowReceiver);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_ZERO_ADDRESS.selector
        );
        constantOutflowNFTProxy.mockMint(address(0), _flowReceiver, nftId);
    }

    function test_RevertIf_InternalMintTokenThatExists(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_TOKEN_ALREADY_EXISTS.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
    }

    function test_RevertIf_InternalMintSameToAndFlowReceiver(
        address _flowSender
    ) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = helper_getNFTId(_flowSender, _flowSender);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowSender, nftId);
    }

    function test_RevertIf_SetApprovalForAllOperatorApproveToCaller(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);
        vm.prank(_flowSender);
        constantOutflowNFTProxy.setApprovalForAll(_flowSender, true);
    }

    function test_RevertIf_ApproveToCurrentOwner(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);
        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_flowSender, nftId);
    }

    function test_RevertIf_ApproveAsNonOwner(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Happy Path Cases
    //////////////////////////////////////////////////////////////////////////*/
    function test_SupportsInterface() public {
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

    function test_ConstantOutflowNFTDeploymentAndStateInitialization() public {
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

    function test_InternalMintToken(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);
    }

    function test_InternalBurnToken(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockBurn(nftId);
        assert_FlowDataState_IsEmpty(nftId);
    }

    function test_Approve(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);
        vm.assume(_flowSender != _approvedAccount);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);

        assert_Approval_IsExpected(nftId, _approvedAccount);
    }

    function test_SetApprovalForAll(
        address _flowSender,
        address _flowReceiver,
        address _operator,
        bool _approved
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowSender != _operator);

        uint256 nftId = helper_getNFTId(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_FlowDataState_IsExpected(nftId, _flowSender, _flowReceiver);

        vm.prank(_flowSender);
        constantOutflowNFTProxy.setApprovalForAll(_operator, _approved);

        assert_OperatorApproval_IsExpected(_flowSender, _operator, _approved);
    }
}
