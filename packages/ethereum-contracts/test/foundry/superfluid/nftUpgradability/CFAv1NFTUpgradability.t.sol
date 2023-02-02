// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { Test } from "forge-std/Test.sol";

import { UUPSProxy } from "../../../../contracts/upgradability/UUPSProxy.sol";
import {
    UUPSProxiable
} from "../../../../contracts/upgradability/UUPSProxiable.sol";
import {
    ISuperfluid,
    ISuperToken
} from "../../../../contracts/interfaces/superfluid/ISuperToken.sol";
import {
    CFAv1NFTBase,
    ConstantInflowNFT
} from "../../../../contracts/superfluid/ConstantInflowNFT.sol";
import {
    ConstantOutflowNFT
} from "../../../../contracts/superfluid/ConstantOutflowNFT.sol";
import { CFAv1BaseTest } from "../CFAv1NFTBase.t.sol";
import { FoundrySuperfluidTester } from "../../FoundrySuperfluidTester.sol";

import {
    CFAv1NFTBaseMockV1,
    CFAv1NFTBaseMockVGoodUpgrade,
    CFAv1NFTBaseMockV1BadNewVariablePreGap,
    CFAv1NFTBaseMockV1BadReorderingPreGap,
    CFAv1NFTBaseMockV1BadPostGap,
    ConstantOutflowNFTMockV1,
    ConstantOutflowNFTMockV1BaseBadNewVariable,
    ConstantOutflowNFTMockV1BadNewVariable,
    ConstantOutflowNFTMockV1GoodUpgrade,
    ICFAv1NFTBaseMockErrors
} from "./CFAv1NFTMocks.sol";

/// @title ConstantFAv1NFTsUpgradabilityTest
/// @author Superfluid
/// @notice Used for testing upgradability of CFAv1 NFT contracts
/// @dev Add a test for new NFT logic contracts here when it changes
contract ConstantFAv1NFTsUpgradabilityTest is CFAv1BaseTest {
    UUPSProxy proxy;
    CFAv1NFTBaseMockV1 cfaV1NFTBaseMockV1Logic;
    CFAv1NFTBaseMockV1 cfaV1NFTBaseMockV1Proxy;

    function setUp() public override {
        super.setUp();
        proxy = new UUPSProxy();
        cfaV1NFTBaseMockV1Logic = new CFAv1NFTBaseMockV1();
        proxy.initializeProxy(address(cfaV1NFTBaseMockV1Logic));
        cfaV1NFTBaseMockV1Proxy = CFAv1NFTBaseMockV1(address(proxy));
        cfaV1NFTBaseMockV1Proxy.initialize(
            superToken,
            "FTTx CFAv1NFTBase",
            "FTTx BASE"
        );

        // Baseline assertion that logic address is expected
        assert_Expected_Logic_Contract_Address(
            cfaV1NFTBaseMockV1Proxy,
            address(cfaV1NFTBaseMockV1Logic)
        );

        vm.prank(governanceOwner);
        superToken.initializeNFTContracts(
            address(cfaV1NFTBaseMockV1Proxy),
            address(cfaV1NFTBaseMockV1Proxy),
            address(0),
            address(0)
        );

        // Baseline passing validate layout for NFT base contract
        cfaV1NFTBaseMockV1Proxy.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Helper Functions
    //////////////////////////////////////////////////////////////////////////*/
    function _helper_Deploy_Mock_Constant_Outflow_NFT()
        internal
        returns (ConstantOutflowNFTMockV1 mockProxy)
    {
        UUPSProxy _proxy = new UUPSProxy();
        ConstantOutflowNFTMockV1 initialOutflowLogicMock = new ConstantOutflowNFTMockV1();
        _proxy.initializeProxy(address(initialOutflowLogicMock));
        mockProxy = ConstantOutflowNFTMockV1(address(_proxy));
        mockProxy.initialize(superToken, "FTTx ConstantOutflowNFT", "FTTx COF");

        // Baseline assertion that logic address is expected
        assert_Expected_Logic_Contract_Address(
            mockProxy,
            address(initialOutflowLogicMock)
        );

        vm.prank(governanceOwner);
        superToken.initializeNFTContracts(
            address(_proxy),
            address(cfaV1NFTBaseMockV1Proxy),
            address(0),
            address(0)
        );

        // Baseline passing validate layout for outflow NFT contract
        mockProxy.validateStorageLayout();
    }

    function helper_Expect_Revert_When_Storage_Layout_Is_Changed(
        string memory _variableName
    ) internal {
        vm.expectRevert(
            abi.encodeWithSelector(
                ICFAv1NFTBaseMockErrors.STORAGE_LOCATION_CHANGED.selector,
                _variableName
            )
        );
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
                                    Revert Tests
    //////////////////////////////////////////////////////////////////////////*/

    function test_Revert_If_NFT_Contract_Upgrade_Is_Not_Executed_By_Host()
        public
    {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT();
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_ONLY_HOST.selector);
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));

        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT();
        vm.expectRevert(CFAv1NFTBase.CFA_NFT_ONLY_HOST.selector);
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }

    function test_Revert_If_You_Upgrade_With_The_Wrong_Logic_Contract() public {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT();
        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT();


        vm.prank(address(sf.host));
        vm.expectRevert("UUPSProxiable: not compatible logic");
        constantOutflowNFTProxy.updateCode(address(newInflowLogic));

        vm.prank(address(sf.host));
        vm.expectRevert("UUPSProxiable: not compatible logic");
        constantInflowNFTProxy.updateCode(address(newOutflowLogic));

    }

    // Should not be able to update CFAv1NFTBase by adding new storage variables in between existing storage
    function test_Revert_If_A_New_Variable_Is_Added_Pre_Storage_Gap_In_Base_NFT_Contract()
        external
    {
        CFAv1NFTBaseMockV1BadNewVariablePreGap badNewLogic = new CFAv1NFTBaseMockV1BadNewVariablePreGap();
        vm.prank(address(superToken.getHost()));
        cfaV1NFTBaseMockV1Proxy.updateCode(address(badNewLogic));

        assert_Expected_Logic_Contract_Address(
            cfaV1NFTBaseMockV1Proxy,
            address(badNewLogic)
        );

        helper_Expect_Revert_When_Storage_Layout_Is_Changed("_name");

        cfaV1NFTBaseMockV1Proxy.validateStorageLayout();
    }

    // Should not be able to reorder CFAv1NFTBase storage variables
    function test_Revert_If_Variables_Are_Reordered_In_Base_NFT_Contract()
        external
    {
        CFAv1NFTBaseMockV1BadReorderingPreGap badNewLogic = new CFAv1NFTBaseMockV1BadReorderingPreGap();
        vm.prank(address(superToken.getHost()));
        cfaV1NFTBaseMockV1Proxy.updateCode(address(badNewLogic));

        assert_Expected_Logic_Contract_Address(
            cfaV1NFTBaseMockV1Proxy,
            address(badNewLogic)
        );

        helper_Expect_Revert_When_Storage_Layout_Is_Changed("_tokenApprovals");

        cfaV1NFTBaseMockV1Proxy.validateStorageLayout();
    }

    // Should not be able to update CFAv1NFTBase by adding new storage variables after gap space
    function test_Revert_If_Variable_Added_After_Storage_Gap_In_Base_NFT_Contract()
        external
    {
        ConstantOutflowNFTMockV1 mockProxy = _helper_Deploy_Mock_Constant_Outflow_NFT();

        ConstantOutflowNFTMockV1BaseBadNewVariable badLogic = new ConstantOutflowNFTMockV1BaseBadNewVariable();
        vm.prank(address(superToken.getHost()));
        mockProxy.updateCode(address(badLogic));

        assert_Expected_Logic_Contract_Address(mockProxy, address(badLogic));

        helper_Expect_Revert_When_Storage_Layout_Is_Changed(
            "_flowDataByTokenId"
        );

        mockProxy.validateStorageLayout();
    }

    // Should be able to update ConstantOutflowNFT by adding new storage variables after mapping
    function test_Passing_If_Outflow_NFT_Is_Upgraded_Properly() external {
        ConstantOutflowNFTMockV1 mockProxy = _helper_Deploy_Mock_Constant_Outflow_NFT();
        ConstantOutflowNFTMockV1GoodUpgrade goodLogic = new ConstantOutflowNFTMockV1GoodUpgrade();

        vm.prank(address(superToken.getHost()));
        mockProxy.updateCode(address(goodLogic));

        assert_Expected_Logic_Contract_Address(mockProxy, address(goodLogic));

        mockProxy.validateStorageLayout();
    }

    // Should not be able to update ConstantOutflowNFT by adding new storage variables before mapping
    function test_Revert_If_A_New_Variable_Is_Added_Incorrectly_To_Outflow_NFT()
        external
    {
        ConstantOutflowNFTMockV1 mockProxy = _helper_Deploy_Mock_Constant_Outflow_NFT();

        ConstantOutflowNFTMockV1BadNewVariable badLogic = new ConstantOutflowNFTMockV1BadNewVariable();
        vm.prank(address(superToken.getHost()));
        mockProxy.updateCode(address(badLogic));

        assert_Expected_Logic_Contract_Address(mockProxy, address(badLogic));

        helper_Expect_Revert_When_Storage_Layout_Is_Changed(
            "_flowDataByTokenId"
        );

        mockProxy.validateStorageLayout();
    }

    /*//////////////////////////////////////////////////////////////////////////
                                    Passing Tests
    //////////////////////////////////////////////////////////////////////////*/

    // Should be able to update CFAv1NFTBase by adding new storage variables in gap space and reducing storage gap by one
    function test_Passing_Base_NFT_Contract_Is_Upgraded_Properly() external {
        CFAv1NFTBaseMockVGoodUpgrade goodNewLogic = new CFAv1NFTBaseMockVGoodUpgrade();
        vm.prank(address(superToken.getHost()));
        cfaV1NFTBaseMockV1Proxy.updateCode(address(goodNewLogic));

        assert_Expected_Logic_Contract_Address(
            cfaV1NFTBaseMockV1Proxy,
            address(goodNewLogic)
        );
    }

    function test_Passing_NFT_Contracts_Can_Be_Upgraded_By_Host() public {
        ConstantOutflowNFT newOutflowLogic = new ConstantOutflowNFT();
        vm.prank(address(sf.host));
        constantOutflowNFTProxy.updateCode(address(newOutflowLogic));
        
        ConstantInflowNFT newInflowLogic = new ConstantInflowNFT();
        vm.prank(address(sf.host));
        constantInflowNFTProxy.updateCode(address(newInflowLogic));
    }
}
