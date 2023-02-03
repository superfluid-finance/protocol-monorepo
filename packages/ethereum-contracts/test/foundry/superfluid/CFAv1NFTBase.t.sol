// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import {
    CFAv1NFTBase,
    ConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import {
    ConstantInflowNFT
} from "../../../contracts/superfluid/ConstantInflowNFT.sol";

import {
    CFAv1Library,
    FoundrySuperfluidTester
} from "../FoundrySuperfluidTester.sol";

contract ConstantOutflowNFTMock is ConstantOutflowNFT {
    /// @dev a mock mint function that exposes the internal _mint function
    function mockMint(
        address _to,
        address _flowReceiver,
        uint256 _newTokenId
    ) public {
        _mint(_to, _flowReceiver, _newTokenId);
    }

    /// @dev a mock burn function that exposes the internal _burn function
    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    /// @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }
}

contract ConstantInflowNFTMock is ConstantInflowNFT {
    /// @dev a mock mint function to emit the mint Transfer event
    function mockMint(address _to, uint256 _newTokenId) public {
        _mint(_to, _newTokenId);
    }

    /// @dev a mock burn function to emit the burn Transfer event
    function mockBurn(uint256 _tokenId) public {
        _burn(_tokenId);
    }

    // @dev this ownerOf doesn't revert if _tokenId doesn't exist
    function mockOwnerOf(uint256 _tokenId) public view returns (address) {
        return _ownerOf(_tokenId);
    }

    /// @dev this exposes the internal flow data by token id for testing purposes
    function mockFlowDataByTokenId(
        uint256 _tokenId
    ) public view returns (FlowData memory flowData) {
        return flowDataByTokenId(_tokenId);
    }
}

abstract contract CFAv1BaseTest is FoundrySuperfluidTester {
    using CFAv1Library for CFAv1Library.InitData;

    string constant OUTFLOW_NFT_NAME_TEMPLATE = " Constant Outflow NFT";
    string constant OUTFLOW_NFT_SYMBOL_TEMPLATE = "COF";
    string constant INFLOW_NFT_NAME_TEMPLATE = " Constant Inflow NFT";
    string constant INFLOW_NFT_SYMBOL_TEMPLATE = "CIF";

    ConstantOutflowNFTMock public constantOutflowNFTLogic;
    ConstantOutflowNFTMock public constantOutflowNFTProxy;
    ConstantInflowNFTMock public constantInflowNFTLogic;
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
        // run setup from FoundrySuperfluidTester
        super.setUp();
        // then deploy contracts
        (
            constantOutflowNFTLogic,
            constantOutflowNFTProxy,
            constantInflowNFTLogic,
            constantInflowNFTProxy
        ) = helper_Deploy_NFT_Contracts_And_Set_Address_In_Super_Token();
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
        CFAv1NFTBase.FlowData memory flowData = constantOutflowNFTProxy
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
        CFAv1NFTBase _nftContract,
        uint256 _tokenId,
        address _expectedOwner,
        bool _isOutflow
    ) public {
        CFAv1NFTBase.FlowData memory flowData = constantOutflowNFTProxy
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
        CFAv1NFTBase _nftContract,
        uint256 _tokenId,
        address _expectedApproved
    ) public {
        address approved = _nftContract.getApproved(_tokenId);

        assertEq(approved, _expectedApproved);
    }

    function assert_OperatorApproval_IsExpected(
        CFAv1NFTBase _nftContract,
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

    function helper_Deploy_Constant_Outflow_NFT()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy
        )
    {
        _constantOutflowNFTLogic = new ConstantOutflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantOutflowNFTLogic));

        _constantOutflowNFTProxy = ConstantOutflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantOutflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, OUTFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, OUTFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_Deploy_Constant_Inflow_NFT()
        public
        returns (
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        _constantInflowNFTLogic = new ConstantInflowNFTMock();
        UUPSProxy proxy = new UUPSProxy();
        proxy.initializeProxy(address(_constantInflowNFTLogic));

        _constantInflowNFTProxy = ConstantInflowNFTMock(address(proxy));
        string memory symbol = superToken.symbol();
        _constantInflowNFTProxy.initialize(
            superToken,
            string.concat(symbol, INFLOW_NFT_NAME_TEMPLATE),
            string.concat(symbol, INFLOW_NFT_SYMBOL_TEMPLATE)
        );
    }

    function helper_Deploy_NFT_Contracts_And_Set_Address_In_Super_Token()
        public
        returns (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy,
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        )
    {
        (
            _constantOutflowNFTLogic,
            _constantOutflowNFTProxy
        ) = helper_Deploy_Constant_Outflow_NFT();
        (
            _constantInflowNFTLogic,
            _constantInflowNFTProxy
        ) = helper_Deploy_Constant_Inflow_NFT();

        vm.prank(sf.governance.owner());
        superToken.initializeNFTContracts(
            address(_constantOutflowNFTProxy),
            address(_constantInflowNFTProxy),
            address(0),
            address(0)
        );
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

        vm.prank(_flowSender);
        sf.cfaLib.createFlow(_flowReceiver, superToken, _flowRate);
        assert_Flow_Data_State_IsExpected(
            nftId,
            _flowSender,
            uint32(block.timestamp),
            _flowReceiver
        );

        (uint256 timestamp, int96 flowRate, , ) = superToken.getFlow(
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
    ) public {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_NFT_Contracts_And_Super_Token_Are_Properly_Initialized()
        public
    {
        (
            ConstantOutflowNFTMock _constantOutflowNFTLogic,
            ConstantOutflowNFTMock _constantOutflowNFTProxy,
            ConstantInflowNFTMock _constantInflowNFTLogic,
            ConstantInflowNFTMock _constantInflowNFTProxy
        ) = helper_Deploy_NFT_Contracts_And_Set_Address_In_Super_Token();
        assertEq(
            address(_constantOutflowNFTProxy),
            address(superToken.constantOutflowNFT())
        );
        assertEq(
            address(_constantInflowNFTProxy),
            address(superToken.constantInflowNFT())
        );
    }
}
