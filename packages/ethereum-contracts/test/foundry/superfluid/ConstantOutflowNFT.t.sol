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

import {
    CFAv1Library,
    FoundrySuperfluidTester
} from "../FoundrySuperfluidTester.sol";
import { CFAv1BaseTest, ConstantOutflowNFTMock } from "./CFAv1NFTBase.t.sol";

contract ConstantOutflowNFTTest is CFAv1BaseTest {
    using CFAv1Library for CFAv1Library.InitData;

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Revert_If_Contract_Already_Initialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantOutflowNFTProxy.initialize(
            superToken,
            string.concat("henlo", OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_Fuzz_Revert_If_Owner_Of_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.ownerOf(_tokenId);
    }

    function test_Fuzz_Revert_If_Get_Approved_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.getApproved(_tokenId);
    }

    function test_Fuzz_Revert_If_Not_Inflow_NFT_Calling_Inflow_Transfer_Mint(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_ONLY_CONSTANT_INFLOW.selector
        );
        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.inflowTransferMint(
            _flowSender,
            _flowReceiver,
            nftId
        );
    }

    function test_Fuzz_Revert_If_Not_Inflow_NFT_Calling_Inflow_Transfer_Burn(
        address _flowSender,
        address _flowReceiver
    ) public {
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_ONLY_CONSTANT_INFLOW.selector
        );
        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.inflowTransferBurn(nftId);
    }

    function test_Fuzz_Revert_If_Internal_Burn_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.mockBurn(_tokenId);
    }

    function test_Fuzz_Revert_If_Internal_Mint_To_Zero_Address(
        address _flowReceiver
    ) public {
        uint256 nftId = helper_Get_NFT_ID(address(0), _flowReceiver);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_ZERO_ADDRESS.selector
        );
        constantOutflowNFTProxy.mockMint(address(0), _flowReceiver, nftId);
    }

    function test_Fuzz_Revert_If_Internal_Mint_Token_That_Exists(
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
            ConstantOutflowNFT.COF_NFT_TOKEN_ALREADY_EXISTS.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
    }

    function test_Fuzz_Revert_If_Internal_Mint_Same_To_And_Flow_Receiver(
        address _flowSender
    ) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowSender);
        vm.expectRevert(
            ConstantOutflowNFT.COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME.selector
        );
        constantOutflowNFTProxy.mockMint(_flowSender, _flowSender, nftId);
    }

    function test_Fuzz_Revert_If_Set_Approval_For_All_Operator_Approve_To_Caller(
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
        vm.prank(_flowSender);
        constantOutflowNFTProxy.setApprovalForAll(_flowSender, true);
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
        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_flowSender, nftId);
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
        /// @dev _flowSender is owner of outflow NFT
        vm.assume(_approver != _flowSender);
        vm.assume(_approvedAccount != _flowSender);

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_APPROVE_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_approver);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);
    }

    function test_Fuzz_Revert_If_You_Try_To_Transfer_Outflow_NFT(
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

        vm.prank(_flowSender);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.prank(_flowSender);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.safeTransferFrom(
            _flowSender,
            _flowReceiver,
            nftId
        );

        vm.prank(_flowSender);
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.safeTransferFrom(
            _flowSender,
            _flowReceiver,
            nftId,
            "0x"
        );
    }

    function test_Fuzz_Revert_If_You_Are_Not_The_Owner_And_Try_To_Transfer_Outflow_NFT(
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

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.safeTransferFrom(
            _flowSender,
            _flowReceiver,
            nftId
        );

        vm.expectRevert(
            CFAv1NFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.safeTransferFrom(
            _flowSender,
            _flowReceiver,
            nftId,
            "0x"
        );
    }

    function test_Revert_When_Create_Flow_Overflows_Because_Timestamp_Is_Greater_Than_Uint32_Max()
        public
    {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;

        vm.warp(type(uint64).max);

        vm.expectRevert(ConstantOutflowNFT.COF_NFT_OVERFLOW.selector);
        constantOutflowNFTProxy.mockMint(
            flowSender,
            flowReceiver,
            helper_Get_NFT_ID(flowSender, flowReceiver)
        );
    }

    function test_Fuzz_Revert_If_On_Create_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(
            caller,
            address(constantOutflowNFTProxy)
        );
        vm.prank(caller);
        vm.expectRevert(ConstantOutflowNFT.COF_NFT_ONLY_CFA.selector);
        constantOutflowNFTProxy.onCreate(address(1), address(2));
    }

    function test_Fuzz_Revert_If_On_Update_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(
            caller,
            address(constantOutflowNFTProxy)
        );
        vm.prank(caller);
        vm.expectRevert(ConstantOutflowNFT.COF_NFT_ONLY_CFA.selector);
        constantOutflowNFTProxy.onUpdate(address(1), address(2));
    }

    function test_Fuzz_Revert_If_On_Delete_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(
            caller,
            address(constantOutflowNFTProxy)
        );
        vm.prank(caller);
        vm.expectRevert(ConstantOutflowNFT.COF_NFT_ONLY_CFA.selector);
        constantOutflowNFTProxy.onDelete(address(1), address(2));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_Contract_Supports_Expected_Interfaces() public {
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

    function test_Passing_Proxiable_UUID_Is_Expected_Value() public {
        assertEq(
            constantOutflowNFTProxy.proxiableUUID(),
            keccak256(
                "org.superfluid-finance.contracts.ConstantOutflowNFT.implementation"
            )
        );
    }

    function test_Passing_Get_No_Flow_Token_URI() public {
        uint256 nftId = helper_Get_NFT_ID(alice, bob);
        assertEq(
            constantOutflowNFTProxy.tokenURI(nftId),
            constantInflowNFTProxy.tokenURI(nftId)
        );
    }

    function test_Fuzz_Passing_NFT_Balance_Of_Is_Always_One(
        address _owner
    ) public {
        assertEq(constantInflowNFTProxy.balanceOf(_owner), 1);
    }

    function test_Passing_Constant_Outflow_NFT_Is_Properly_Initialized()
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
            address(constantOutflowNFTProxy),
            _flowSender,
            address(0),
            nftId
        );

        constantOutflowNFTProxy.mockBurn(nftId);
        assert_Flow_Data_State_IsEmpty(nftId);
    }

    function test_Fuzz_Passing_Inflow_Mint_Is_Called_By_Inflow_NFT(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );
        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        vm.prank(address(constantInflowNFTProxy));
        constantOutflowNFTProxy.inflowTransferMint(
            _flowSender,
            _flowReceiver,
            nftId
        );
    }

    function test_Fuzz_Passing_Inflow_Burn_Is_Called_By_Inflow_NFT(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );
        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);

        vm.prank(address(constantInflowNFTProxy));
        constantOutflowNFTProxy.inflowTransferBurn(nftId);
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
        vm.assume(_flowSender != _approvedAccount);

        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);
        constantOutflowNFTProxy.mockMint(_flowSender, _flowReceiver, nftId);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        assert_Event_Approval(
            address(constantOutflowNFTProxy),
            _flowSender,
            _approvedAccount,
            nftId
        );

        vm.prank(_flowSender);
        constantOutflowNFTProxy.approve(_approvedAccount, nftId);

        assert_Approval_IsExpected(
            constantOutflowNFTProxy,
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
            address(constantOutflowNFTProxy),
            _tokenOwner,
            _operator,
            _approved
        );

        vm.prank(_tokenOwner);
        constantOutflowNFTProxy.setApprovalForAll(_operator, _approved);

        assert_OperatorApproval_IsExpected(
            constantOutflowNFTProxy,
            _tokenOwner,
            _operator,
            _approved
        );
    }

    function test_Passing_Create_Flow_Mints_Outflow_And_Inflow_NFTs_And_Emits_Transfer_Events()
        public
    {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        helper_Create_Flow_And_Assert_NFT_Invariants(
            flowSender,
            flowReceiver,
            flowRate
        );
    }

    function test_Passing_Update_Flow_Does_Not_Impact_Storage_And_Emits_MetadataUpdate_Events()
        public
    {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        helper_Create_Flow_And_Assert_NFT_Invariants(
            flowSender,
            flowReceiver,
            flowRate
        );

        uint256 nftId = helper_Get_NFT_ID(flowSender, flowReceiver);
        assert_Event_MetadataUpdate(address(constantOutflowNFTProxy), nftId);
        assert_Event_MetadataUpdate(address(constantInflowNFTProxy), nftId);

        vm.prank(flowSender);
        sf.cfaLib.updateFlow(flowReceiver, superToken, flowRate + 333);

        assert_Flow_Data_State_IsExpected(
            nftId,
            flowSender,
            uint32(block.timestamp),
            flowReceiver
        );
    }

    function test_Passing_Delete_Flow_Clears_Storage_And_Emits_Transfer_Events()
        public
    {
        int96 flowRate = 42069;
        address flowSender = alice;
        address flowReceiver = bob;
        helper_Create_Flow_And_Assert_NFT_Invariants(
            flowSender,
            flowReceiver,
            flowRate
        );

        uint256 nftId = helper_Get_NFT_ID(flowSender, flowReceiver);

        assert_Event_Transfer(
            address(constantInflowNFTProxy),
            flowReceiver,
            address(0),
            nftId
        );

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            flowSender,
            address(0),
            nftId
        );

        vm.prank(flowSender);
        sf.cfaLib.deleteFlow(flowSender, flowReceiver, superToken);

        assert_Flow_Data_State_IsEmpty(nftId);
    }
}
