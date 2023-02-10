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
import { CFAv1BaseTest } from "./CFAv1NFTBase.t.sol";


contract ConstantInflowNFTTest is CFAv1BaseTest {
    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Revert_If_Contract_Already_Initialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantInflowNFTProxy.initialize(
            superToken,
            string.concat("henlo", INFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_Fuzz_Revert_If_Owner_Of_Called_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.ownerOf(_tokenId);
    }

    function test_Fuzz_Revert_If_Get_Approved_Called_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantInflowNFTProxy.getApproved(_tokenId);
    }

    function test_Fuzz_Revert_If_Approve_To_Caller_When_Set_Approval_For_All(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.setApprovalForAll(_flowReceiver, true);
    }

    function test_Fuzz_Revert_If_Approve_To_Current_Owner(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(CFAv1NFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_flowReceiver, nftId);
    }

    function test_Fuzz_Revert_If_Approve_As_Non_Owner(
        address _flowSender,
        address _flowReceiver,
        address _approver,
        address _approvedAccount
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );
        /// @dev _flowReceiver is owner of inflow NFT
        vm.assume(_approver != _flowReceiver);
        vm.assume(_approvedAccount != _flowReceiver);

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_approver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);
    }

    function test_Fuzz_Revert_If_You_Try_To_Transfer_Inflow_NFT(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            address(0),
            _flowSender,
            nftId
        );

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        vm.prank(_flowReceiver);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.transferFrom(_flowReceiver, _flowSender, nftId);

        vm.prank(_flowReceiver);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.safeTransferFrom(
            _flowReceiver,
            _flowSender,
            nftId
        );

        vm.prank(_flowReceiver);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantInflowNFTProxy.safeTransferFrom(
            _flowReceiver,
            _flowSender,
            nftId,
            "0x"
        );
    }

    function test_Fuzz_Revert_If_You_Are_Not_The_Owner_And_Try_To_Transfer_Inflow_NFT(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowSender);
        constantInflowNFTProxy.transferFrom(_flowReceiver, _flowSender, nftId);

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowSender);
        constantInflowNFTProxy.safeTransferFrom(
            _flowReceiver,
            _flowSender,
            nftId
        );

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowSender);
        constantInflowNFTProxy.safeTransferFrom(
            _flowReceiver,
            _flowSender,
            nftId,
            "0x"
        );
    }

    function test_Fuzz_Revert_If_Mint_Is_Not_Called_By_Outflow_NFT(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(
            caller,
            address(constantOutflowNFTProxy)
        );
        vm.expectRevert(
            ConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector
        );
        constantInflowNFTProxy.mint(address(0), 69);
    }

    function test_Fuzz_Revert_If_Burn_Is_Not_Called_By_Outflow_NFT(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(
            caller,
            address(constantOutflowNFTProxy)
        );
        vm.expectRevert(
            ConstantInflowNFT.CIF_NFT_ONLY_CONSTANT_OUTFLOW.selector
        );
        constantInflowNFTProxy.burn(69);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_Contract_Supports_Expected_Interfaces() public {
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

    function test_Passing_Proxiable_UUID_Is_Expected_Value() public {
        assertEq(
            constantInflowNFTProxy.proxiableUUID(),
            keccak256(
                "org.superfluid-finance.contracts.ConstantInflowNFT.implementation"
            )
        );
    }

    function test_Fuzz_Passing_NFT_Balance_Of_Is_Always_One(
        address _owner
    ) public {
        assertEq(constantInflowNFTProxy.balanceOf(_owner), 1);
    }

    function test_Passing_Constant_Inflow_NFT_Is_Properly_Initialized() public {
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

    function test_Fuzz_Passing_FlowData_By_Token_Id_Mint(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        CFAv1NFTBase.CFAv1NFTFlowData memory flowData = constantInflowNFTProxy
            .mockCFAv1NFTFlowDataByTokenId(nftId);
        assertEq(flowData.flowSender, _flowSender);
        assertEq(flowData.flowReceiver, _flowReceiver);
    }

    function test_Fuzz_Passing_Internal_Mint_Token(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        assert_Event_Transfer(
            address(constantInflowNFTProxy),
            address(0),
            _flowReceiver,
            nftId
        );

        constantInflowNFTProxy.mockMint(_flowReceiver, nftId);

        assert_Flow_Data_State_IsEmpty(nftId);
    }

    function test_Fuzz_Passing_Internal_Burn_Token(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        assert_Event_Transfer(
            address(constantInflowNFTProxy),
            _flowReceiver,
            address(0),
            nftId
        );

        constantInflowNFTProxy.mockBurn(nftId);

        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );
    }

    function test_Fuzz_Passing_Approve(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );
        vm.assume(_flowReceiver != _approvedAccount);

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        assert_Event_Approval(
            address(constantInflowNFTProxy),
            _flowReceiver,
            _approvedAccount,
            nftId
        );

        vm.prank(_flowReceiver);
        constantInflowNFTProxy.approve(_approvedAccount, nftId);

        assert_Approval_IsExpected(
            constantInflowNFTProxy,
            nftId,
            _approvedAccount
        );
    }

    function test_Fuzz_Passing_Set_Approval_For_All(
        address _tokenOwner,
        address _operator,
        bool _approved
    ) public {
        vm.assume(_tokenOwner != address(0));
        vm.assume(_tokenOwner != _operator);

        assert_Event_ApprovalForAll(
            address(constantInflowNFTProxy),
            _tokenOwner,
            _operator,
            _approved
        );

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
