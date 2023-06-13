// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import {
    FlowNFTBase,
    IFlowNFTBase,
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { PoolAdminNFT, IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { PoolMemberNFT, IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";
import { FoundrySuperfluidTester } from "../FoundrySuperfluidTester.sol";
import { ConstantOutflowNFTMock, ConstantInflowNFTMock } from "../../../contracts/mocks/CFAv1NFTMock.sol";
import { SuperToken, SuperTokenMock } from "../../../contracts/mocks/SuperTokenMock.sol";
import { TestToken } from "../../../contracts/utils/TestToken.sol";
import {
    FlowNFTBaseStorageLayoutMock,
    ConstantInflowNFTStorageLayoutMock,
    ConstantOutflowNFTStorageLayoutMock
} from "../../../contracts/mocks/CFAv1NFTUpgradabilityMock.sol";

abstract contract FlowNFTBaseTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperTokenMock;
    using SuperTokenV1Library for SuperToken;

    string constant internal OUTFLOW_NFT_NAME_TEMPLATE = "Constant Outflow NFT";
    string constant internal OUTFLOW_NFT_SYMBOL_TEMPLATE = "COF";
    string constant internal INFLOW_NFT_NAME_TEMPLATE = "Constant Inflow NFT";
    string constant internal INFLOW_NFT_SYMBOL_TEMPLATE = "CIF";

    SuperTokenMock public superTokenMock;

    ConstantOutflowNFTMock public constantOutflowNFTLogic;
    ConstantInflowNFTMock public constantInflowNFTLogic;

    ConstantOutflowNFTMock public constantOutflowNFTProxy;
    ConstantInflowNFTMock public constantInflowNFTProxy;

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);

    event Approval(address indexed owner, address indexed approved, uint256 indexed tokenId);

    event ApprovalForAll(address indexed owner, address indexed operator, bool approved);

    event MetadataUpdate(uint256 _tokenId);

    constructor() FoundrySuperfluidTester(5) { }

    function setUp() public virtual override {
        super.setUp();

        // deploy outflow NFT contract
        UUPSProxy outflowProxy = new UUPSProxy();

        // deploy inflow NFT contract
        UUPSProxy inflowProxy = new UUPSProxy();

        // we deploy mock NFT contracts for the tests to access internal functions
        constantOutflowNFTLogic = new ConstantOutflowNFTMock(
            sf.host,
            IConstantInflowNFT(address(inflowProxy))
        );
        constantInflowNFTLogic = new ConstantInflowNFTMock(
            sf.host,
            IConstantOutflowNFT(address(outflowProxy))
        );

        constantOutflowNFTLogic.castrate();
        constantInflowNFTLogic.castrate();

        // initialize proxy to point at logic
        outflowProxy.initializeProxy(address(constantOutflowNFTLogic));

        // initialize proxy to point at logic
        inflowProxy.initializeProxy(address(constantInflowNFTLogic));

        constantOutflowNFTProxy = ConstantOutflowNFTMock(address(outflowProxy));
        constantInflowNFTProxy = ConstantInflowNFTMock(address(inflowProxy));

        constantOutflowNFTProxy.initialize("Constant Outflow NFT", "COF");

        constantInflowNFTProxy.initialize("Constant Inflow NFT", "CIF");

        // Deploy TestToken
        TestToken testTokenMock = new TestToken(
            "Mock Test",
            "MT",
            18,
            100000000
        );

        // Deploy SuperToken proxy
        UUPSProxy superTokenMockProxy = new UUPSProxy();

        // deploy super token mock for testing with mock constant outflow/inflow NFTs
        SuperTokenMock superTokenMockLogic = new SuperTokenMock(
            sf.host,
            0,
            IConstantOutflowNFT(address(constantOutflowNFTProxy)),
            IConstantInflowNFT(address(constantInflowNFTProxy)),
            // @note use address(0) for now
            IPoolAdminNFT(address(0)),
            IPoolMemberNFT(address(0))
        );
        superTokenMockProxy.initializeProxy(address(superTokenMockLogic));

        superTokenMock = SuperTokenMock(address(superTokenMockProxy));
        superTokenMock.initialize(testTokenMock, 18, "Super Mock Test", "MTx");

        // mint tokens to test accounts
        for (uint256 i = 0; i < N_TESTERS; i++) {
            superTokenMock.mintInternal(TEST_ACCOUNTS[i], INIT_SUPER_TOKEN_BALANCE, "0x", "0x");
        }

        vm.prank(sf.governance.owner());
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function _assertNFTFlowDataStateIsExpected(
        uint256 _tokenId,
        address _expectedSuperToken,
        address _expectedFlowSender,
        uint32 _expectedFlowStartDate,
        address _expectedFlowReceiver
    ) public {
        FlowNFTBase.FlowNFTData memory flowData = constantOutflowNFTProxy.flowDataByTokenId(_tokenId);

        assertEq(flowData.superToken, _expectedSuperToken);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowSender, _expectedFlowSender);

        // assert flow start date is equal to expected flow start date
        assertEq(flowData.flowStartDate, _expectedFlowStartDate);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowReceiver, _expectedFlowReceiver);

        // assert owner of outflow nft equal to expected flow sender
        _assertOwnerOf(constantOutflowNFTProxy, _tokenId, _expectedFlowSender, true);

        // assert owner of inflow nft equal to expected flow receiver
        _assertOwnerOf(constantInflowNFTProxy, _tokenId, _expectedFlowReceiver, false);
    }

    function _assertNFTFlowDataStateIsEmpty(uint256 _tokenId) public {
        _assertNFTFlowDataStateIsExpected(_tokenId, address(0), address(0), 0, address(0));
    }

    function _assertOwnerOf(FlowNFTBase _nftContract, uint256 _tokenId, address _expectedOwner, bool _isOutflow)
        public
    {
        address actualOwner = _isOutflow
            ? ConstantOutflowNFTMock(address(_nftContract)).mockOwnerOf(_tokenId)
            : ConstantInflowNFTMock(address(_nftContract)).mockOwnerOf(_tokenId);

        assertEq(actualOwner, _expectedOwner);
    }

    function _assertApprovalIsExpected(FlowNFTBase _nftContract, uint256 _tokenId, address _expectedApproved) public {
        address approved = _nftContract.getApproved(_tokenId);

        assertEq(approved, _expectedApproved);
    }

    function _assertOperatorApprovalIsExpected(
        FlowNFTBase _nftContract,
        address _expectedOwner,
        address _expectedOperator,
        bool _expectedOperatorApproval
    ) public {
        bool operatorApproval = _nftContract.isApprovedForAll(_expectedOwner, _expectedOperator);

        assertEq(operatorApproval, _expectedOperatorApproval);
    }

    function _assertEventTransfer(
        address _emittingAddress,
        address _expectedFrom,
        address _expectedTo,
        uint256 _expectedTokenId
    ) public {
        vm.expectEmit(true, true, true, false, _emittingAddress);

        emit Transfer(_expectedFrom, _expectedTo, _expectedTokenId);
    }

    function _assertEventApproval(
        address _emittingAddress,
        address _expectedOwner,
        address _expectedApproved,
        uint256 _expectedTokenId
    ) public {
        vm.expectEmit(true, true, true, false, _emittingAddress);

        emit Approval(_expectedOwner, _expectedApproved, _expectedTokenId);
    }

    function _assertEventApprovalForAll(
        address _emittingAddress,
        address _expectedOwner,
        address _expectedOperator,
        bool _expectedApproved
    ) public {
        vm.expectEmit(true, true, false, true, _emittingAddress);

        emit ApprovalForAll(_expectedOwner, _expectedOperator, _expectedApproved);
    }

    function _assertEventMetadataUpdate(address _emittingAddress, uint256 _tokenId) public {
        vm.expectEmit(true, false, false, false, _emittingAddress);

        emit MetadataUpdate(_tokenId);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/
    function _helperGetNFTID(address _superToken, address _flowSender, address _flowReceiver)
        public
        view
        returns (uint256)
    {
        return constantOutflowNFTProxy.getTokenId(_superToken, _flowSender, _flowReceiver);
    }

    function _helperCreateFlowAndAssertNFTInvariants(address _flowSender, address _flowReceiver, int96 _flowRate)
        public
    {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFTProxy), address(0), _flowSender, nftId);

        _assertEventTransfer(address(constantInflowNFTProxy), address(0), _flowReceiver, nftId);

        vm.startPrank(_flowSender);
        superTokenMock.createFlow(_flowReceiver, _flowRate);
        vm.stopPrank();
        _assertNFTFlowDataStateIsExpected(
            nftId, address(superTokenMock), _flowSender, uint32(block.timestamp), _flowReceiver
        );

        (uint256 timestamp, int96 flowRate,,) = sf.cfa.getFlow(superTokenMock, _flowSender, _flowReceiver);
        assertEq(timestamp, block.timestamp);
        assertEq(flowRate, _flowRate);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assume Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function _assumeSenderNEQReceiverAndNeitherAreZeroAddress(address _flowSender, address _flowReceiver) public pure {
        vm.assume(_flowSender != address(0));
        vm.assume(_flowReceiver != address(0));
        vm.assume(_flowSender != _flowReceiver);
    }

    function _assumeCallerIsNotOtherAddress(address caller, address otherAddress) public pure {
        vm.assume(caller != otherAddress);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testCFAv1IsProperlySetDuringInitialization() public {
        assertEq(address(constantOutflowNFTProxy.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
        assertEq(address(constantInflowNFTProxy.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
    }
}

/// @title ConstantFAv1NFTsUpgradabilityTest
/// @author Superfluid
/// @notice Used for testing storage layout of CFAv1 NFT contracts
contract ConstantFAv1NFTsUpgradabilityTest is FlowNFTBaseTest {
    function setUp() public override {
        super.setUp();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Assertion Helpers
    //////////////////////////////////////////////////////////////////////////*/
    function _assertExpectedLogicContractAddress(UUPSProxiable _proxy, address _expectedLogicContract) public {
        assertEq(_proxy.getCodeAddress(), _expectedLogicContract);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Storage Layout Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testStorageLayoutOfFlowNFTBase() public {
        FlowNFTBaseStorageLayoutMock flowNFTBaseStorageLayoutMock = new FlowNFTBaseStorageLayoutMock(
                sf.host
            );
        flowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function testStorageLayoutOfConstantInflowNFT() public {
        ConstantInflowNFTStorageLayoutMock constantInflowNFTBaseStorageLayoutMock =
        new ConstantInflowNFTStorageLayoutMock(
                sf.host,
                constantOutflowNFTProxy
            );
        constantInflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function testStorageLayoutOfConstantOutflowNFT() public {
        ConstantOutflowNFTStorageLayoutMock constantOutflowNFTBaseStorageLayoutMock =
        new ConstantOutflowNFTStorageLayoutMock(
                sf.host,
                constantInflowNFTProxy
            );
        constantOutflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testRevertNFTContractsCannotBeUpgradedByNonHost(address notSuperTokenFactory) public {
        vm.assume(notSuperTokenFactory != address(sf.superTokenFactory));
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(
            sf.host,
            constantInflowNFTProxy
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(
            sf.host,
            constantOutflowNFTProxy
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testNFTContractsCanBeUpgradedByHost() public {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(
            sf.host,
            constantInflowNFTProxy
        );
        vm.prank(address(sf.superTokenFactory));
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(
            sf.host,
            constantOutflowNFTProxy
        );
        vm.prank(address(sf.superTokenFactory));
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }
}
