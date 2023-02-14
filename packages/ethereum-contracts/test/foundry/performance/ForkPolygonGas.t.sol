// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.18;

import { console, Test } from "forge-std/Test.sol";
import {
    IConstantFlowAgreementV1
} from "../../../contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
import {
    IInstantDistributionAgreementV1
} from "../../../contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    IERC20,
    ISuperToken
} from "../../../contracts/interfaces/superfluid/ISuperToken.sol";
import { SuperTokenV1Library } from "../../../contracts/apps/SuperTokenV1Library.sol";

/// @title ForkPolygonGasTest 
/// @author Superfluid
/// @notice A test contract to measure gas consumption of common Superfluid operations
/// on a forked network
/// @dev Use forge snapshot --match-contract ForkPolygonGasTest to run this test
/// You can also compare the gas of two different deployments by specifying the block
/// Then running forge snapshot --diff --match-contract ForkPolygonGasTest to see the diff
contract ForkPolygonGasTest is Test {
    using SuperTokenV1Library for ISuperToken;
    uint256 polygonFork;

    string POLYGON_MAINNET_PROVIDER_URL = vm.envString("POLYGON_MAINNET_PROVIDER_URL");

    ISuperfluid public constant host =
        ISuperfluid(0x3E14dC1b13c488a8d5D310918780c983bD5982E7);
    IERC20 public constant weth =
        IERC20(0x7ceB23fD6bC0adD59E62ac25578270cFf1b9f619);
    ISuperToken public constant ethX =
        ISuperToken(0x27e1e4E6BC79D93032abef01025811B7E4727e85);
    address public constant TEST_ACCOUNT =
        0x0154d25120Ed20A516fE43991702e7463c5A6F6e;
    address public constant ALICE = address(1);
    address public constant BOB = address(2);
    address public constant DEFAULT_FLOW_OPERATOR = address(69);

    // uint256 marketingNFTDeploymentBlock = 34616690;

    function setUp() public {
        vm.startPrank(TEST_ACCOUNT);
        polygonFork = vm.createSelectFork(POLYGON_MAINNET_PROVIDER_URL);

        // vm.rollFork(marketingNFTDeploymentBlock);

        // these functions are executed in set up so we can get an accurate gas estimate in the
        // test functions

        // approve max amount to upgrade
        weth.approve(address(ethX), type(uint256).max);

        // create a flow
        ethX.createFlow(ALICE, 4206933);

        // approve operator permissions
        ethX.setMaxFlowPermissions(DEFAULT_FLOW_OPERATOR);
    }

    function assert_Flow_Rate_Is_Expected(
        address sender,
        address receiver,
        int96 expectedFlowRate
    ) public {
        (, int96 flowRate, , ) = ethX.getFlowInfo(sender, receiver);
        assertEq(flowRate, expectedFlowRate);
    }

    function test_Polygon_Fork() public {
        assertEq(vm.activeFork(), polygonFork);
    }

    function test_Retrieve_Balance() public {
        uint256 balance = ethX.balanceOf(TEST_ACCOUNT);
    }

    function test_Downgrade() public {
        uint256 superTokenBalanceBefore = ethX.balanceOf(TEST_ACCOUNT);
        uint256 expectedBalance = superTokenBalanceBefore - 4206933;
        ethX.downgrade(4206933);
        uint256 superTokenBalanceAfter = ethX.balanceOf(TEST_ACCOUNT);
        assertEq(superTokenBalanceAfter, expectedBalance);
    }

    function test_Gas_Downgrade() public {
        ethX.downgrade(4206933);
    }

    function test_Upgrade() public {
        uint256 superTokenBalanceBefore = ethX.balanceOf(TEST_ACCOUNT);
        uint256 expectedBalance = superTokenBalanceBefore + 4206933;
        ethX.upgrade(4206933);
        uint256 superTokenBalanceAfter = ethX.balanceOf(TEST_ACCOUNT);
        assertEq(superTokenBalanceAfter, expectedBalance);
    }

    function test_Gas_Upgrade() public {
        ethX.upgrade(4206933);
    }

    function test_Create_Flow() public {
        ethX.createFlow(BOB, 4206933);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, BOB, 4206933);
    }

    function test_Update_Flow() public {
        ethX.updateFlow(ALICE, 694201337);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, ALICE, 694201337);
    }

    function test_Delete_Flow() public {
        ethX.deleteFlow(TEST_ACCOUNT, ALICE);
        assert_Flow_Rate_Is_Expected(TEST_ACCOUNT, ALICE, 0);
    }
    function test_Gas_Create_Flow() public {
        ethX.createFlow(BOB, 4206933);
    }

    function test_Gas_Update_Flow() public {
        ethX.updateFlow(ALICE, 4206933);
    }

    function test_Gas_Delete_Flow() public {
        ethX.deleteFlow(TEST_ACCOUNT, ALICE);
    }

    function test_Gas_Authorize_Flow_Operator_Permissions() public {
        ethX.setMaxFlowPermissions(ALICE);
    }

    function test_Gas_Revoke_Flow_Operator_Permissions() public {
        ethX.revokeFlowPermissions(DEFAULT_FLOW_OPERATOR);
    }

    function test_Gas_Create_Index() public {
        ethX.createIndex(1);
    }
}
