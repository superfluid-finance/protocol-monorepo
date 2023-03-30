// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { Test } from "forge-std/Test.sol";

import {
    UUPSProxiable
} from "../../../../contracts/upgradability/UUPSProxiable.sol";
import {
    FlowNFTBase,
    ConstantInflowNFT
} from "../../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    IFlowNFTBase
} from "../../../../contracts/interfaces/superfluid/IFlowNFTBase.sol";
import {
    ConstantOutflowNFT
} from "../../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { FlowNFTBaseTest } from "../FlowNFTBase.t.sol";
import { FoundrySuperfluidTester } from "../../FoundrySuperfluidTester.sol";
import {
    FlowNFTBaseStorageLayoutMock,
    ConstantInflowNFTStorageLayoutMock,
    ConstantOutflowNFTStorageLayoutMock
} from "./CFAv1NFTUpgradabilityMocks.sol";

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
    function assert_Expected_Logic_Contract_Address(
        UUPSProxiable _proxy,
        address _expectedLogicContract
    ) public {
        assertEq(_proxy.getCodeAddress(), _expectedLogicContract);
    }

    /*//////////////////////////////////////////////////////////////////////////
                                Storage Layout Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Storage_Layout_Of_FlowNFTBase() public {
        FlowNFTBaseStorageLayoutMock flowNFTBaseStorageLayoutMock = new FlowNFTBaseStorageLayoutMock(
                sf.cfa
            );
        flowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function test_Storage_Layout_Of_ConstantInflowNFT() public {
        ConstantInflowNFTStorageLayoutMock constantInflowNFTBaseStorageLayoutMock = new ConstantInflowNFTStorageLayoutMock(
                sf.cfa
            );
        constantInflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    function test_Storage_Layout_Of_ConstantOutflowNFT() public {
        ConstantOutflowNFTStorageLayoutMock constantOutflowNFTBaseStorageLayoutMock = new ConstantOutflowNFTStorageLayoutMock(
                sf.cfa
            );
        constantOutflowNFTBaseStorageLayoutMock.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Revert_NFT_Contracts_Cannot_Be_Upgraded_By_Non_Host(
        address notHost
    ) public {
        vm.assume(notHost != address(sf.host));
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(sf.cfa);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_HOST.selector);
        vm.prank(notHost);
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(sf.cfa);
        vm.expectRevert(IFlowNFTBase.CFA_NFT_ONLY_HOST.selector);
        vm.prank(notHost);
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/
    function test_Passing_NFT_Contracts_Can_Be_Upgraded_By_Host() public {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT(sf.cfa);
        vm.prank(address(sf.host));
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT(sf.cfa);
        vm.prank(address(sf.host));
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }
}
