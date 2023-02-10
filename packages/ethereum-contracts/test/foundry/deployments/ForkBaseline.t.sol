// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { console, Test } from "forge-std/Test.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import {
    IConstantFlowAgreementV1
} from "../../../contracts/agreements/ConstantFlowAgreementV1.sol";
import {
    IInstantDistributionAgreementV1
} from "../../../contracts/interfaces/agreements/IInstantDistributionAgreementV1.sol";
import { IResolver } from "../../../contracts/interfaces/utils/IResolver.sol";
import {
    ISuperfluidGovernance
} from "../../../contracts/interfaces/superfluid/ISuperfluidGovernance.sol";
import {
    SuperfluidLoader
} from "../../../contracts/utils/SuperfluidLoader.sol";
import {
    ISuperfluid
} from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";
import {
    IERC20,
    ISuperToken,
    SafeERC20
} from "../../../contracts/superfluid/SuperToken.sol";
import {
    ISuperTokenFactory
} from "../../../contracts/superfluid/SuperTokenFactory.sol";
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

     struct SuperfluidFramework {
        ISuperfluid host;
        IInstantDistributionAgreementV1 idaV1;
        IConstantFlowAgreementV1 cfaV1;
        SuperfluidLoader superfluidLoader;
        ISuperTokenFactory superTokenFactory;
        ISuperfluidGovernance governance;
        ISuperToken token;
    }

    struct ExpectedIndexData {
        address publisher;
        uint32 indexId;
        bool exist;
        uint128 indexValue;
        uint128 totalUnitsApproved;
        uint128 totalUnitsPending;
    }

    struct ExpectedSubscriptionData {
        address publisher;
        uint32 indexId;
        address subscriber;
        bool approved;
        bool exist;
        uint128 units;
        uint256 unitsPendingDistribution;
    }

    address public adminPrankAccount;
    ISuperToken public superToken;
    SuperfluidFramework public sfFramework;
    uint256 public snapshot;

    constructor(
        ISuperToken _superToken,
        address _adminPrankAccount,
        IResolver resolver,
        string memory providerURLKey
    ) {
        string memory providerURL = vm.envString(providerURLKey);
        vm.createSelectFork(providerURL);

        adminPrankAccount = _adminPrankAccount;
        superToken = _superToken;
        SuperfluidLoader superfluidLoader = SuperfluidLoader(
            resolver.get("SuperfluidLoader-v1")
        );
        SuperfluidLoader.Framework memory framework = superfluidLoader
            .loadFramework("v1");

        sfFramework = SuperfluidFramework({
            host: framework.superfluid,
            idaV1: IInstantDistributionAgreementV1(
                address(framework.agreementIDAv1)
            ),
            cfaV1: IConstantFlowAgreementV1(address(framework.agreementCFAv1)),
            superfluidLoader: superfluidLoader,
            superTokenFactory: framework.superTokenFactory,
            governance: framework.superfluid.getGovernance(),
            token: _superToken
        });
        snapshot = vm.snapshot();
    }

    function assert_Expected_Flow_Rate(
        ISuperToken _superToken,
        address sender,
        address receiver,
        int96 expectedFlowRate
    ) public {
        (, int96 flowRate, , ) = _superToken.getFlowInfo(sender, receiver);
        assertEq(flowRate, expectedFlowRate, "flow rate not equal");
    }

    function assert_Balance_Is_Expected(
        IERC20 _token,
        address account,
        uint256 expectedBalance
    ) public {
        assertEq(_token.balanceOf(account), expectedBalance, "token balance not equal");
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

    function assert_Expected_Index_Data(
        ISuperToken _superToken,
        ExpectedIndexData memory expectedIndexData
    ) public {
        (
            bool exist,
            uint128 indexValue,
            uint128 totalUnitsApproved,
            uint128 totalUnitsPending
        ) = _superToken.getIndex(
                expectedIndexData.publisher,
                expectedIndexData.indexId
            );

        assertEq(exist, expectedIndexData.exist, "index data: exist not equal");
        assertEq(indexValue, expectedIndexData.indexValue, "index data: indexValue not equal");
        assertEq(totalUnitsApproved, expectedIndexData.totalUnitsApproved, "index data: totalUnitsApproved not equal");
        assertEq(totalUnitsPending, expectedIndexData.totalUnitsPending, "index data: totalUnitsPending not equal");
    }

    function assert_Expected_Subscription_Data(
        ISuperToken _superToken,
        ExpectedSubscriptionData memory expectedSubscriptionData
    ) public {
        (
            bool exist,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        ) = _superToken.getSubscription(
                expectedSubscriptionData.publisher,
                expectedSubscriptionData.indexId,
                expectedSubscriptionData.subscriber
            );

        assertEq(expectedSubscriptionData.exist, exist, "subscription data: exist not equal");
        assertEq(expectedSubscriptionData.units, units, "subscription data: units not equal");
        assertEq(expectedSubscriptionData.unitsPendingDistribution, approved ? 0 : pendingDistribution, "subscription data: pending distribution not equal");
    }

    function helper_Create_Update_Delete_Flow_One_To_One(
        ISuperToken _superToken,
        address prankedAccount
    ) public {
        // test that flows can still be created with SuperTokenFactory updated
        vm.startPrank(prankedAccount);

        _superToken.createFlow(address(1), 42069);
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 42069);

        _superToken.updateFlow(address(1), 4206933);
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 4206933);

        _superToken.deleteFlow(prankedAccount, address(1));
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 0);

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
        helper_Set_Max_Flow_Permissions(_superToken, prankedAccount);
        vm.startPrank(address(1));
        _superToken.createFlowFrom(prankedAccount, address(1), 42069);
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 42069);
        _superToken.updateFlowFrom(prankedAccount, address(1), 4206933);
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 4206933);
        _superToken.deleteFlowFrom(prankedAccount, address(1));
        assert_Expected_Flow_Rate(_superToken, prankedAccount, address(1), 0);
        vm.stopPrank();
    }

    function helper_Create_Non_Upgradeable_Super_Token(
        ISuperTokenFactory superTokenFactory,
        IERC20 underlyingToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);
        superTokenFactory.createERC20Wrapper(
            underlyingToken,
            18,
            ISuperTokenFactory.Upgradability.NON_UPGRADABLE,
            "Super Mr.",
            "MRx"
        );
        vm.stopPrank();
    }

    function helper_Create_Semi_Upgradeable_Super_Token(
        ISuperTokenFactory superTokenFactory,
        IERC20 underlyingToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);
        superTokenFactory.createERC20Wrapper(
            underlyingToken,
            18,
            ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
            "Super Mr.",
            "MRx"
        );
        vm.stopPrank();
    }

    function helper_Create_Fully_Upgradeable_Super_Token(
        ISuperTokenFactory superTokenFactory,
        IERC20 underlyingToken,
        address prankedAccount
    ) public {
        vm.startPrank(prankedAccount);

        superTokenFactory.createERC20Wrapper(
            underlyingToken,
            18,
            ISuperTokenFactory.Upgradability.FULL_UPGRADABLE,
            "Super Mr.",
            "MRx"
        );
        vm.stopPrank();
    }

    function helper_Create_Index(
        ISuperToken _superToken,
        address publisher
    ) public {
        vm.startPrank(publisher);
        _superToken.createIndex(1);
        assert_Expected_Index_Data(
            _superToken,
            ExpectedIndexData({
                publisher: publisher,
                indexId: 1,
                exist: true,
                indexValue: 0,
                totalUnitsApproved: 0,
                totalUnitsPending: 0
            })
        );
        vm.stopPrank();
    }

    function helper_Update_Index_Value(
        ISuperToken _superToken,
        address publisher,
        uint128 newIndexValue
    ) public {
        (
            bool exist,
            uint128 indexValue,
            uint128 totalUnitsApproved,
            uint128 totalUnitsPending
        ) = _superToken.getIndex(
                publisher,
                1
            );
        vm.startPrank(publisher);
        _superToken.updateIndexValue(1, newIndexValue);
        assert_Expected_Index_Data(
            _superToken,
            ExpectedIndexData({
                publisher: publisher,
                indexId: 1,
                exist: true,
                indexValue: newIndexValue,
                totalUnitsApproved: totalUnitsApproved,
                totalUnitsPending: totalUnitsPending
            })
        );

        vm.stopPrank();
    }

    function helper_Distribute(
        ISuperToken _superToken,
        address publisher,
        uint256 distributionAmount
    ) public {
        vm.startPrank(publisher);
        _superToken.distribute(1, distributionAmount);
        vm.stopPrank();
    }

    function helper_Approve_Subscription(
        ISuperToken _superToken,
        address publisher,
        address subscriber
    ) public {
        (, , uint128 units, uint256 pendingDistribution) = _superToken
            .getSubscription(publisher, 1, subscriber);
        vm.startPrank(subscriber);
        _superToken.approveSubscription(publisher, 1);
        assert_Expected_Subscription_Data(
            _superToken,
            ExpectedSubscriptionData({
                publisher: publisher,
                subscriber: subscriber,
                indexId: 1,
                exist: true,
                units: units,
                approved: true,
                unitsPendingDistribution: pendingDistribution
            })
        );
        vm.stopPrank();
    }

    function helper_Revoke_Subscription(
        ISuperToken _superToken,
        address publisher,
        address subscriber
    ) public {
        (, bool approved, uint128 units, uint256 pendingDistribution) = _superToken
            .getSubscription(publisher, 1, subscriber);
        vm.startPrank(subscriber);
        _superToken.revokeSubscription(publisher, 1);
        assert_Expected_Subscription_Data(
            _superToken,
            ExpectedSubscriptionData({
                publisher: publisher,
                subscriber: subscriber,
                indexId: 1,
                exist: true,
                units: units,
                approved: false,
                unitsPendingDistribution: pendingDistribution
            })
        );
        vm.stopPrank();
    }

    function helper_Update_Subscription_Units(
        ISuperToken _superToken,
        address publisher,
        address subscriber,
        uint128 newSubscriptionUnits
    ) public {
        (
            ,
            bool approved,
            uint128 units,
            uint256 pendingDistribution
        ) = _superToken.getSubscription(publisher, 1, subscriber);
        vm.startPrank(publisher);
        _superToken.updateSubscriptionUnits(
            1,
            subscriber,
            newSubscriptionUnits
        );
        assert_Expected_Subscription_Data(
            _superToken,
            ExpectedSubscriptionData({
                publisher: publisher,
                subscriber: subscriber,
                indexId: 1,
                exist: true,
                units: newSubscriptionUnits,
                approved: approved,
                unitsPendingDistribution: 0 
            })
        );

        vm.stopPrank();
    }

    function helper_Delete_Subscription(
        ISuperToken _superToken,
        address publisher,
        address subscriber
    ) public {
        vm.startPrank(publisher);
        _superToken.deleteSubscription(publisher, 1, subscriber);
        assert_Expected_Subscription_Data(
            _superToken,
            ExpectedSubscriptionData({
                publisher: publisher,
                subscriber: subscriber,
                indexId: 1,
                exist: false,
                units: 0,
                approved: false,
                unitsPendingDistribution: 0
            })
        );
        vm.stopPrank();
    }

    function helper_Claim(
        ISuperToken _superToken,
        address publisher,
        address subscriber
    ) public {
        (bool exist, bool approved, uint128 units, uint256 pendingDistribution) = _superToken
            .getSubscription(publisher, 1, subscriber);
        vm.startPrank(subscriber);
        _superToken.claim(publisher, 1, subscriber);
        assert_Expected_Subscription_Data(
            _superToken,
            ExpectedSubscriptionData({
                publisher: publisher,
                subscriber: subscriber,
                indexId: 1,
                exist: exist,
                units: units,
                approved: approved,
                unitsPendingDistribution: 0
            })
        );
        vm.stopPrank();
    }

    function helper_Run_Full_Baseline_Tests() public {
        // SuperToken Baseline Tests
        helper_Wrap_Unwrap(superToken, adminPrankAccount);

        // SuperTokenFactory Baseline Tests
        ERC20 mrToken = new ERC20("Mr. Token", "MR");
        helper_Create_Non_Upgradeable_Super_Token(
            sfFramework.superTokenFactory,
            mrToken,
            adminPrankAccount
        );

        helper_Create_Semi_Upgradeable_Super_Token(
            sfFramework.superTokenFactory,
            mrToken,
            adminPrankAccount
        );

        helper_Create_Fully_Upgradeable_Super_Token(
            sfFramework.superTokenFactory,
            mrToken,
            adminPrankAccount
        );

        // ConstantFlowAgreementV1 Baseline Tests
        helper_Create_Update_Delete_Flow_One_To_One(
            superToken,
            adminPrankAccount
        );

        helper_Set_Flow_Permissions(superToken, adminPrankAccount);

        helper_Set_Max_Flow_Permissions(superToken, adminPrankAccount);

        helper_Set_Revoke_Flow_Permissions(superToken, adminPrankAccount);

        helper_ACL_Create_Update_Delete_Flow_One_To_One(
            superToken,
            adminPrankAccount
        );

        // InstantDistributionAgreementV1 Baseline Tests
        // create index
        helper_Create_Index(superToken, adminPrankAccount);
        
        // update subscription units to three addresses
        helper_Update_Subscription_Units(superToken, adminPrankAccount, address(1), 1);
        helper_Update_Subscription_Units(superToken, adminPrankAccount, address(2), 1);
        helper_Update_Subscription_Units(superToken, adminPrankAccount, address(3), 1);
        
        // approve two subscriptions
        helper_Approve_Subscription(superToken, adminPrankAccount, address(1));
        helper_Approve_Subscription(superToken, adminPrankAccount, address(3));
        
        // revoke one subscription of the approved
        helper_Revoke_Subscription(superToken, adminPrankAccount, address(3));
        
        // delete the revoked subscription
        helper_Delete_Subscription(superToken, adminPrankAccount, address(3));
        
        // update the index value
        helper_Update_Index_Value(superToken, adminPrankAccount, 420);
        
        // claim a subscription
        helper_Claim(superToken, adminPrankAccount, address(2));
        
        // // execute a distribution
        helper_Distribute(superToken, adminPrankAccount, 420);
        
        // // claim a subscription
        helper_Claim(superToken, adminPrankAccount, address(2));
    }

    /// @notice A suite of baseline tests to be run after an upgrade
    /// @dev This is run after constructor and setUp is run
    function test_Passing_Run_Full_Baseline_Tests_Post_Upgrade() public {
        helper_Run_Full_Baseline_Tests();
    }
}
