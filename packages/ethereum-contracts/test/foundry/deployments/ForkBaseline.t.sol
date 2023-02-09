// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { console, Test } from "forge-std/Test.sol";
import {
    IERC20,
    ISuperToken
} from "../../../contracts/superfluid/SuperToken.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

/// @title ForkBaselineTest
/// @author Superfluid
/// @notice A contract containing a set of baseline tests to be used in forked mainnet tests
/// @dev This contract contains a baseline test function which will run the standard Superfluid
/// operations on a forked mainnet. It will also make public some of the baseline test functions
/// so that they can be used between stages of the upgrade process.
contract ForkBaselineTest is Test {
    using SuperTokenV1Library for ISuperToken;

    address public adminPrankAccount;
    ISuperToken public superToken;

    constructor(ISuperToken _superToken, address _adminPrankAccount) {
        adminPrankAccount = _adminPrankAccount;
        superToken = _superToken;
    }

    function assert_Flow_Rate_Is_Expected(
        ISuperToken _superToken,
        address sender,
        address receiver,
        int96 expectedFlowRate
    ) public {
        (, int96 flowRate, , ) = _superToken.getFlowInfo(sender, receiver);
        assertEq(flowRate, expectedFlowRate);
    }

    function assert_Balance_Is_Expected(
        IERC20 _token,
        address account,
        uint256 expectedBalance
    ) public {
        assertEq(_token.balanceOf(account), expectedBalance);
    }

    function assert_Flow_Permissions(
        ISuperToken token,
        address sender,
        address flowOperator,
        bool expectedAllowCreate,
        bool expectedAllowUpdate,
        bool expectedAllowDelete,
        int96 expectedFlowRateAllowance
    ) public {
        (
            bool allowCreate,
            bool allowUpdate,
            bool allowDelete,
            int96 flowRateAllowance
        ) = token.getFlowPermissions(sender, flowOperator);
        (, int96 flowRate, , ) = token.getFlowInfo(sender, flowOperator);
        assertEq(allowCreate, expectedAllowCreate);
        assertEq(allowUpdate, expectedAllowUpdate);
        assertEq(allowDelete, expectedAllowDelete);
    }

    function helper_Create_Update_Delete_Flow_One_To_One(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        // test that flows can still be created with SuperTokenFactory updated
        vm.startPrank(prankedAccount);

        _superToken.createFlow(address(1), 42069);
        assert_Flow_Rate_Is_Expected(_superToken, prankedAccount, address(1), 42069);

        _superToken.updateFlow(address(1), 4206933);
        assert_Flow_Rate_Is_Expected(_superToken, prankedAccount, address(1), 4206933);

        _superToken.deleteFlow(prankedAccount, address(1));
        assert_Flow_Rate_Is_Expected(_superToken, prankedAccount, address(1), 0);

        vm.stopPrank();
    }

    function helper_Wrap_Unwrap(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        // test that flows can still be created with SuperTokenFactory updated
        vm.startPrank(prankedAccount);
        IERC20 underlyingToken = IERC20(_superToken.getUnderlyingToken());
        uint256 underlyingTokenBalanceBefore = underlyingToken.balanceOf(
            prankedAccount
        );
        uint256 superTokenBalanceBefore = _superToken.balanceOf(prankedAccount);
        _superToken.upgrade(42069);
        assert_Balance_Is_Expected(
            underlyingToken,
            prankedAccount,
            underlyingTokenBalanceBefore - 42069
        );
        assert_Balance_Is_Expected(
            _superToken,
            prankedAccount,
            superTokenBalanceBefore + 42069
        );

        _superToken.downgrade(420691);
        assert_Balance_Is_Expected(
            underlyingToken,
            prankedAccount,
            underlyingTokenBalanceBefore - 42069 + 420691
        );
        assert_Balance_Is_Expected(
            _superToken,
            prankedAccount,
            superTokenBalanceBefore + 42069 - 420691
        );

        vm.stopPrank();
    }

    function helper_Set_Flow_Permissions(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);
        _superToken.setFlowPermissions(address(1), true, true, true, 42069);
        assert_Flow_Permissions(
            _superToken,
            prankedAccount,
            address(1),
            true,
            true,
            true,
            42069
        );
        vm.stopPrank();
    }

    function helper_Set_Max_Flow_Permissions(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);
        _superToken.setMaxFlowPermissions(address(1));
        assert_Flow_Permissions(
            _superToken,
            prankedAccount,
            address(1),
            true,
            true,
            true,
            type(int96).max
        );
        vm.stopPrank();
    }

    function helper_Set_Revoke_Flow_Permissions(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        _superToken.revokeFlowPermissions(address(1));
        assert_Flow_Permissions(
            _superToken,
            prankedAccount,
            address(1),
            false,
            false,
            false,
            0
        );

        vm.stopPrank();
    }

    function helper_ACL_Create_Update_Delete_Flow_One_To_One(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        vm.stopPrank();
    }

    function helper_Create_Non_Upgradeable_Super_Token(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        vm.stopPrank();
    }

    function helper_Create_Semi_Upgradeable_Super_Token(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        vm.stopPrank();
    }

    function helper_Create_Fully_Upgradeable_Super_Token(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        vm.stopPrank();
    }

    function helper_Run_Full_Baseline_Tests() public {
        helper_Create_Update_Delete_Flow_One_To_One(
            superToken,
            adminPrankAccount
        );
        helper_Wrap_Unwrap(superToken, adminPrankAccount);
        helper_Set_Flow_Permissions(superToken, adminPrankAccount);
        helper_Set_Max_Flow_Permissions(superToken, adminPrankAccount);
        helper_Set_Revoke_Flow_Permissions(superToken, adminPrankAccount);
        helper_Set_Flow_Permissions(superToken, adminPrankAccount);
    }

    /// @notice A suite of baseline tests to be run after an upgrade
    /// @dev This test suite
    function test_Passing_Run_Full_Baseline_Tests_Post_Upgrade() public {
        helper_Run_Full_Baseline_Tests();
    }
}
