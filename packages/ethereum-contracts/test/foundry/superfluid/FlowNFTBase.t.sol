// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    FlowNFTBase,
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT,
    IConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import {
    ConstantOutflowNFTMock,
    ConstantInflowNFTMock
} from "./CFAv1NFTMock.t.sol";
import {
    SuperToken,
    SuperTokenMock
} from "../../../contracts/mocks/SuperTokenMock.sol";

abstract contract FlowNFTBaseTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperTokenMock;
    using SuperTokenV1Library for SuperToken;

    string constant OUTFLOW_NFT_NAME_TEMPLATE = " Constant Outflow NFT";
    string constant OUTFLOW_NFT_SYMBOL_TEMPLATE = "COF";
    string constant INFLOW_NFT_NAME_TEMPLATE = " Constant Inflow NFT";
    string constant INFLOW_NFT_SYMBOL_TEMPLATE = "CIF";

    SuperTokenMock public superTokenMock;

    ConstantOutflowNFTMock public constantOutflowNFTLogic;
    ConstantInflowNFTMock public constantInflowNFTLogic;

    ConstantOutflowNFTMock public constantOutflowNFTProxy;
    ConstantInflowNFTMock public constantInflowNFTProxy;

    event Transfer(
        address indexed from,
        address indexed to,
        uint256 indexed tokenId
    );

    event Approval(
        address indexed owner,
        address indexed approved,
        uint256 indexed tokenId
    );

    event ApprovalForAll(
        address indexed owner,
        address indexed operator,
        bool approved
    );

    event MetadataUpdate(uint256 _tokenId);

    constructor() FoundrySuperfluidTester(5) {}

    function setUp() public virtual override {
        super.setUp();

        // we deploy mock NFT contracts for the tests to access internal functions
        constantOutflowNFTLogic = new ConstantOutflowNFTMock(sf.cfa);
        constantInflowNFTLogic = new ConstantInflowNFTMock(sf.cfa);

        // deploy super token mock for testing
        superTokenMock = new SuperTokenMock(
            sf.host,
            0,
            IConstantOutflowNFT(address(constantOutflowNFTLogic)),
            IConstantInflowNFT(address(constantInflowNFTLogic))
        );

        // mint tokens to test accounts
        for (uint256 i = 0; i < N_TESTERS; i++) {
            superTokenMock.mintInternal(
                TEST_ACCOUNTS[i],
                INIT_SUPER_TOKEN_BALANCE,
                "0x",
                "0x"
            );
        }

        string memory symbol = superTokenMock.symbol();
        // deploy outflow NFT contract
        UUPSProxy outflowProxy = new UUPSProxy();
        outflowProxy.initializeProxy(address(constantOutflowNFTLogic));

        constantOutflowNFTProxy = ConstantOutflowNFTMock(address(outflowProxy));
        constantOutflowNFTProxy.initialize(
            superTokenMock,
            string.concat(symbol, OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );

        // deploy inflow NFT contract
        UUPSProxy inflowProxy = new UUPSProxy();
        inflowProxy.initializeProxy(address(constantInflowNFTLogic));

        constantInflowNFTProxy = ConstantInflowNFTMock(address(inflowProxy));
        constantInflowNFTProxy.initialize(
            superTokenMock,
            string.concat(symbol, INFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, INFLOW_NFT_SYMBOL_TEMPLATE)
        );

        vm.prank(sf.governance.owner());
        // set mock nft proxy contracts
        superTokenMock.setNFTProxyContracts(
            address(constantOutflowNFTProxy),
            address(constantInflowNFTProxy),
            address(0),
            address(0)
        );
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function assert_Flow_Data_State_IsExpected(
        uint256 _tokenId,
        address _expectedFlowSender,
        uint32 _expectedFlowStartDate,
        address _expectedFlowReceiver
    ) public {
        FlowNFTBase.FlowNFTData memory flowData = constantOutflowNFTProxy
            .flowDataByTokenId(_tokenId);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowSender, _expectedFlowSender);

        // assert flow start date is equal to expected flow start date
        assertEq(flowData.flowStartDate, _expectedFlowStartDate);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowReceiver, _expectedFlowReceiver);

        // assert owner of outflow nft equal to expected flow sender
        assert_OwnerOf(
            constantOutflowNFTProxy,
            _tokenId,
            _expectedFlowSender,
            true
        );

        // assert owner of inflow nft equal to expected flow receiver
        assert_OwnerOf(
            constantInflowNFTProxy,
            _tokenId,
            _expectedFlowReceiver,
            false
        );
    }

    function assert_Flow_Data_State_IsEmpty(uint256 _tokenId) public {
        assert_Flow_Data_State_IsExpected(_tokenId, address(0), 0, address(0));
    }

    function assert_OwnerOf(
        FlowNFTBase _nftContract,
        uint256 _tokenId,
        address _expectedOwner,
        bool _isOutflow
    ) public {
        FlowNFTBase.FlowNFTData memory flowData = constantOutflowNFTProxy
            .flowDataByTokenId(_tokenId);

        address actualOwner = _isOutflow
            ? ConstantOutflowNFTMock(address(_nftContract)).mockOwnerOf(
                _tokenId
            )
            : ConstantInflowNFTMock(address(_nftContract)).mockOwnerOf(
                _tokenId
            );

        assertEq(actualOwner, _expectedOwner);
    }

    function assert_Approval_IsExpected(
        FlowNFTBase _nftContract,
        uint256 _tokenId,
        address _expectedApproved
    ) public {
        address approved = _nftContract.getApproved(_tokenId);

        assertEq(approved, _expectedApproved);
    }

    function assert_OperatorApproval_IsExpected(
        FlowNFTBase _nftContract,
        address _expectedOwner,
        address _expectedOperator,
        bool _expectedOperatorApproval
    ) public {
        bool operatorApproval = _nftContract.isApprovedForAll(
            _expectedOwner,
            _expectedOperator
        );

        assertEq(operatorApproval, _expectedOperatorApproval);
    }

    function assert_Event_Transfer(
        address _emittingAddress,
        address _expectedFrom,
        address _expectedTo,
        uint256 _expectedTokenId
    ) public {
        vm.expectEmit(true, true, true, false, _emittingAddress);

        emit Transfer(_expectedFrom, _expectedTo, _expectedTokenId);
    }

    function assert_Event_Approval(
        address _emittingAddress,
        address _expectedOwner,
        address _expectedApproved,
        uint256 _expectedTokenId
    ) public {
        vm.expectEmit(true, true, true, false, _emittingAddress);

        emit Approval(_expectedOwner, _expectedApproved, _expectedTokenId);
    }

    function assert_Event_ApprovalForAll(
        address _emittingAddress,
        address _expectedOwner,
        address _expectedOperator,
        bool _expectedApproved
    ) public {
        vm.expectEmit(true, true, false, true, _emittingAddress);

        emit ApprovalForAll(
            _expectedOwner,
            _expectedOperator,
            _expectedApproved
        );
    }

    function assert_Event_MetadataUpdate(
        address _emittingAddress,
        uint256 _tokenId
    ) public {
        vm.expectEmit(true, false, false, false, _emittingAddress);

        emit MetadataUpdate(_tokenId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/
    function helper_Get_NFT_ID(
        address _flowSender,
        address _flowReceiver
    ) public pure returns (uint256) {
        return uint256(keccak256(abi.encode(_flowSender, _flowReceiver)));
    }

    function helper_Create_Flow_And_Assert_NFT_Invariants(
        address _flowSender,
        address _flowReceiver,
        int96 _flowRate
    ) public {
        uint256 nftId = helper_Get_NFT_ID(_flowSender, _flowReceiver);

        assert_Event_Transfer(
            address(constantOutflowNFTProxy),
            address(0),
            _flowSender,
            nftId
        );

        assert_Event_Transfer(
            address(constantInflowNFTProxy),
            address(0),
            _flowReceiver,
            nftId
        );

        // we must start prank if using SuperTokenV1Library syntax
        vm.startPrank(_flowSender);
        superTokenMock.createFlow(_flowReceiver, _flowRate);
        vm.stopPrank();
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        (uint256 timestamp, int96 flowRate, , ) = sf.cfa.getFlow(
            superTokenMock,
            _flowSender,
            _flowReceiver
        );
        assertEq(timestamp, block.timestamp);
        assertEq(flowRate, _flowRate);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function assume_Sender_NEQ_Receiver_And_Neither_Are_The_Zero_Address(
        address _flowSender,
        address _flowReceiver
    ) public pure {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);
    }

    function assume_Caller_Is_Not_Other_Address(
        address caller,
        address otherAddress
    ) public pure {
        vm.assume(caller != otherAddress);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_CFAv1_Is_Properly_Set_During_Initialization() public {
        assertEq(address(constantOutflowNFTProxy.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
        assertEq(address(constantInflowNFTProxy.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
    }
}
