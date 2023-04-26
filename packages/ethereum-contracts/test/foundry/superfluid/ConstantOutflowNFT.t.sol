// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import {
    IERC165,
    IERC721,
    IERC721Metadata
} from "@openzeppelin/contracts/token/ERC721/extensions/IERC721Metadata.sol";
import { Strings } from "@openzeppelin/contracts/utils/Strings.sol";

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    FlowNFTBase,
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    CFAv1Library,
    FoundrySuperfluidTester
} from "../FoundrySuperfluidTester.sol";
import {
    IFlowNFTBase
} from "../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import { FlowNFTBaseTest } from "./FlowNFTBase.t.sol";
import {
    ConstantOutflowNFTMock,
    NoNFTSuperTokenMock
} from "./CFAv1NFTMock.t.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

contract ConstantOutflowNFTTest is FlowNFTBaseTest {
    using Strings for uint256;
    using CFAv1Library for CFAv1Library.InitData;

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Revert_If_Contract_Already_Initialized() public {
        vm.expectRevert("Initializable: contract is already initialized");

        constantOutflowNFTProxy.initialize(
            string.concat("henlo", OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat("goodbye", OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function test_Fuzz_Revert_If_Owner_Of_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.ownerOf(_tokenId);
    }

    function test_Fuzz_Revert_If_Get_Approved_For_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.getApproved(_tokenId);
    }

    function test_Fuzz_Revert_If_Internal_Burn_Non_Existent_Token(
        uint256 _tokenId
    ) public {
        vm.expectRevert(IFlowNFTBase.CFA_NFT_INVALID_TOKEN_ID.selector);
        constantOutflowNFTProxy.mockBurn(_tokenId);
    }

    function test_Fuzz_Revert_If_Internal_Mint_To_Zero_Address(
        address _flowReceiver
    ) public {
        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            address(0),
            _flowReceiver
        );
        vm.expectRevert(
            IConstantOutflowNFT.COF_NFT_MINT_TO_ZERO_ADDRESS.selector
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            address(0),
            _flowReceiver,
            nftId
        );
    }

    function test_Fuzz_Revert_If_Internal_Mint_Token_That_Exists(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        vm.expectRevert(
            IConstantOutflowNFT.COF_NFT_TOKEN_ALREADY_EXISTS.selector
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
    }

    function test_Fuzz_Revert_If_Internal_Mint_Same_To_And_Flow_Receiver(
        address _flowSender
    ) public {
        vm.assume(_flowSender != address(0));

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowSender
        );
        vm.expectRevert(
            IConstantOutflowNFT.COF_NFT_MINT_TO_AND_FLOW_RECEIVER_SAME.selector
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowSender,
            nftId
        );
    }

    function test_Fuzz_Revert_If_Set_Approval_For_All_Operator_Approve_To_Caller(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CALLER.selector);
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_APPROVE_TO_CURRENT_OWNER.selector);
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        vm.expectRevert(
            IFlowNFTBase
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            address(0),
            _flowSender,
            nftId
        );

        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
        constantOutflowNFTProxy.safeTransferFrom(
            _flowSender,
            _flowReceiver,
            nftId
        );

        vm.prank(_flowSender);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_TRANSFER_IS_NOT_ALLOWED.selector);
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            address(0),
            _flowSender,
            nftId
        );

        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        vm.expectRevert(
            IFlowNFTBase
                .CFA_NFT_TRANSFER_CALLER_NOT_OWNER_OR_APPROVED_FOR_ALL
                .selector
        );
        vm.prank(_flowReceiver);
        constantOutflowNFTProxy.transferFrom(_flowSender, _flowReceiver, nftId);

        vm.expectRevert(
            IFlowNFTBase
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
            IFlowNFTBase
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

    function test_Fuzz_Revert_If_On_Create_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(caller, address(sf.cfa));
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        vm.prank(caller);
        constantOutflowNFTProxy.onCreate(superToken, address(1), address(2));
    }

    function test_Fuzz_Revert_If_On_Update_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(caller, address(sf.cfa));
        vm.prank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFTProxy.onUpdate(superToken, address(1), address(2));
    }

    function test_Fuzz_Revert_If_On_Delete_Is_Not_Called_By_CFAv1(
        address caller
    ) public {
        assume_Caller_Is_Not_Other_Address(caller, address(sf.cfa));
        vm.prank(caller);
        vm.expectRevert(IConstantOutflowNFT.COF_NFT_ONLY_FLOW_AGREEMENTS.selector);
        constantOutflowNFTProxy.onDelete(superToken, address(1), address(2));
    }

    function test_Revert_Get_No_Flow_Token_URI() public {
        uint256 nftId = helper_Get_NFT_ID(address(superTokenMock), alice, bob);
        vm.expectRevert();
        constantOutflowNFTProxy.tokenURI(nftId);
        vm.expectRevert();
        constantInflowNFTProxy.tokenURI(nftId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_Contract_Supports_Expected_Interfaces() public {
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC165).interfaceId
            ),
            true
        );
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC721).interfaceId
            ),
            true
        );
        assertEq(
            constantOutflowNFTProxy.supportsInterface(
                type(IERC721Metadata).interfaceId
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

    function test_Fuzz_Passing_NFT_Balance_Of_Is_Always_One(
        address _owner
    ) public {
        assertEq(constantInflowNFTProxy.balanceOf(_owner), 1);
    }

    function test_Passing_Constant_Outflow_NFT_Is_Properly_Initialized()
        public
    {
        assertEq(constantOutflowNFTProxy.name(), OUTFLOW_NFT_NAME_TEMPLATE);
        assertEq(constantOutflowNFTProxy.symbol(), OUTFLOW_NFT_SYMBOL_TEMPLATE);
    }

    function test_Fuzz_Passing_Internal_Mint_Token(
        address _flowSender,
        address _flowReceiver
    ) public {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            address(0),
            _flowSender,
            nftId
        );

        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
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
        assert_NFT_Flow_Data_State_IsEmpty(nftId);
    }

    function test_Fuzz_Passing_Approve(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public returns (uint256 nftId) {
        assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
            _flowSender,
            _flowReceiver
        );
        vm.assume(_flowSender != _approvedAccount);

        nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            _flowSender,
            _flowReceiver
        );
        constantOutflowNFTProxy.mockMint(
            address(superTokenMock),
            _flowSender,
            _flowReceiver,
            nftId
        );
        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
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

    function test_Fuzz_Passing_Approve_Then_Burn(
        address _flowSender,
        address _flowReceiver,
        address _approvedAccount
    ) public {
        uint256 nftId = test_Fuzz_Passing_Approve(
            _flowSender,
            _flowReceiver,
            _approvedAccount
        );
        constantOutflowNFTProxy.mockBurn(nftId);

        assertEq(constantOutflowNFTProxy.mockGetApproved(nftId), address(0));
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            flowSender,
            flowReceiver
        );
        assert_Event_MetadataUpdate(address(constantOutflowNFTProxy), nftId);
        assert_Event_MetadataUpdate(address(constantInflowNFTProxy), nftId);

        vm.prank(flowSender);
        sf.cfaLib.updateFlow(flowReceiver, superTokenMock, flowRate + 333);

        assert_NFT_Flow_Data_State_IsExpected(
            nftId,
            address(superTokenMock),
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            flowSender,
            flowReceiver
        );

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
        sf.cfaLib.deleteFlow(flowSender, flowReceiver, superTokenMock);

        assert_NFT_Flow_Data_State_IsEmpty(nftId);
    }

    function test_Passing_Token_URI_Is_Expected()
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

        uint256 nftId = helper_Get_NFT_ID(
            address(superTokenMock),
            flowSender,
            flowReceiver
        );

        assertEq(
            constantOutflowNFTProxy.tokenURI(nftId),
            string(
                abi.encodePacked(
                    "&token_address=",
                    Strings.toHexString(uint256(uint160(address(superTokenMock))), 20),
                    "?chain_id=",
                    block.chainid.toString(),
                    "&token_symbol=",
                    superTokenMock.symbol(),
                    "&sender=",
                    Strings.toHexString(uint256(uint160(flowSender)), 20),
                    "&receiver=",
                    Strings.toHexString(uint256(uint160(flowReceiver)), 20),
                    "&token_decimals=",
                    uint256(superTokenMock.decimals())
                        .toString(),
                    "&start_date=1", // timestamp shifts 1
                    "&flowRate=",
                    uint256(uint96(flowRate)).toString(),
                    "&is_inflow=false"
                )
            )
        );
    }

    function test_Passing_Create_Update_Delete_Flow_No_NFT_Token() public {
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
        sf.cfaLib.createFlow(bob, noNFTSuperTokenMock, 100);
        (, int96 flowRate, , ) = sf.cfa.getFlow(
            noNFTSuperTokenMock,
            alice,
            bob
        );
        assertEq(flowRate, 100);
        sf.cfaLib.updateFlow(bob, noNFTSuperTokenMock, 150);
        (, flowRate, , ) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 150);
        sf.cfaLib.updateFlow(bob, noNFTSuperTokenMock, 90);
        (, flowRate, , ) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 90);
        sf.cfaLib.deleteFlow(alice, bob, noNFTSuperTokenMock);
        (, flowRate, , ) = sf.cfa.getFlow(noNFTSuperTokenMock, alice, bob);
        assertEq(flowRate, 0);
        vm.stopPrank();
    }
}
