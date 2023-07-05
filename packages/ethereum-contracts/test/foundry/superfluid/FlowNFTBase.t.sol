// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { IERC721Metadata } from "@openzeppelin/contracts/interfaces/IERC721Metadata.sol";
import { UUPSProxy } from "../../../contracts/upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../../../contracts/upgradability/UUPSProxiable.sol";
import {
    FlowNFTBase,
    IFlowNFTBase,
    ConstantOutflowNFT,
    IConstantOutflowNFT
} from "../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT, IConstantInflowNFT } from "../../../contracts/superfluid/ConstantInflowNFT.sol";
import { IPoolAdminNFT } from "../../../contracts/superfluid/PoolAdminNFT.sol";
import { IPoolMemberNFT } from "../../../contracts/superfluid/PoolMemberNFT.sol";
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
import { ERC721IntegrationTest } from "./ERC721.t.sol";

abstract contract FlowNFTBaseTest is ERC721IntegrationTest {
    using SuperTokenV1Library for SuperTokenMock;
    using SuperTokenV1Library for SuperToken;

    function setUp() public virtual override {
        super.setUp();
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
        FlowNFTBase.FlowNFTData memory flowData = constantOutflowNFT.flowDataByTokenId(_tokenId);

        assertEq(flowData.superToken, _expectedSuperToken);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowSender, _expectedFlowSender);

        // assert flow start date is equal to expected flow start date
        assertEq(flowData.flowStartDate, _expectedFlowStartDate);

        // assert flow sender is equal to expected flow sender
        assertEq(flowData.flowReceiver, _expectedFlowReceiver);

        // assert owner of outflow nft equal to expected flow sender
        _assertOwnerOfIsExpected(
            constantOutflowNFT, _tokenId, _expectedFlowSender, "ConstantOutflowNFT: owner of COF nft not as expected"
        );

        // assert owner of inflow nft equal to expected flow receiver
        _assertOwnerOfIsExpected(
            constantInflowNFT, _tokenId, _expectedFlowReceiver, "ConstantInflowNFT: owner of COF nft not as expected"
        );
    }

    function _assertNFTFlowDataStateIsEmpty(uint256 _tokenId) public {
        _assertNFTFlowDataStateIsExpected(_tokenId, address(0), address(0), 0, address(0));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/
    function _helperGetNFTID(address _superToken, address _flowSender, address _flowReceiver)
        public
        view
        returns (uint256)
    {
        return constantOutflowNFT.getTokenId(_superToken, _flowSender, _flowReceiver);
    }

    function _helperCreateFlowAndAssertNFTInvariants(address _flowSender, address _flowReceiver, int96 _flowRate)
        public
    {
        uint256 nftId = _helperGetNFTID(address(superTokenMock), _flowSender, _flowReceiver);

        _assertEventTransfer(address(constantOutflowNFT), address(0), _flowSender, nftId);

        _assertEventTransfer(address(constantInflowNFT), address(0), _flowReceiver, nftId);

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
    function testHostIsProperlySetInConstructor() public {
        assertEq(address(constantOutflowNFT.HOST()), address(sf.host));
        assertEq(address(constantInflowNFT.HOST()), address(sf.host));
    }
    function testCFAv1IsProperlySetInConstructor() public {
        assertEq(address(constantOutflowNFT.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
        assertEq(address(constantInflowNFT.CONSTANT_FLOW_AGREEMENT_V1()), address(sf.cfa));
    }

    function testGDAv1IsProperlySetInConstructor() public {
        assertEq(address(constantOutflowNFT.GENERAL_DISTRIBUTION_AGREEMENT_V1()), address(sf.gda));
        assertEq(address(constantInflowNFT.GENERAL_DISTRIBUTION_AGREEMENT_V1()), address(sf.gda));
    }
}

/// @title CFAv1NFTUpgradabilityTest
/// @author Superfluid
/// @notice Used for testing storage layout and upgradability of CFAv1 NFT contracts
contract CFAv1NFTUpgradabilityTest is FlowNFTBaseTest {
    function setUp() public override {
        super.setUp();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Storage Layout Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testFlowNFTBaseStorageLayout() public {
        FlowNFTBaseStorageLayoutMock flowNFTBaseStorageLayoutMock = new FlowNFTBaseStorageLayoutMock(
                sf.host
            );
        flowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function testConstantInflowNFTStorageLayout() public {
        ConstantInflowNFTStorageLayoutMock constantInflowNFTBaseStorageLayoutMock =
        new ConstantInflowNFTStorageLayoutMock(
                sf.host,
                constantOutflowNFT
            );
        constantInflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function testConstantOutflowNFTStorageLayout() public {
        ConstantOutflowNFTStorageLayoutMock constantOutflowNFTBaseStorageLayoutMock =
        new ConstantOutflowNFTStorageLayoutMock(
                sf.host,
                constantInflowNFT
            );
        constantOutflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testRevertFlowNFTContractsCannotBeUpgradedByNonSuperTokenFactory(address notSuperTokenFactory) public {
        vm.assume(notSuperTokenFactory != address(sf.superTokenFactory));
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(
            sf.host,
            constantInflowNFT
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        constantOutflowNFT.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(
            sf.host,
            constantOutflowNFT
        );
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_SUPER_TOKEN_FACTORY.selector);
        vm.prank(notSuperTokenFactory);
        constantInflowNFT.updateCode(address(newInflowLogic));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function testFlowNFTContractsCanBeUpgradedBySuperTokenFactory() public {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(
            sf.host,
            constantInflowNFT
        );
        vm.prank(address(sf.superTokenFactory));
        constantOutflowNFT.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(
            sf.host,
            constantOutflowNFT
        );
        vm.prank(address(sf.superTokenFactory));
        constantInflowNFT.updateCode(address(newInflowLogic));
    }
}
