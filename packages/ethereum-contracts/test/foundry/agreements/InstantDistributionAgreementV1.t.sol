// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { SuperToken } from "../../../contracts/superfluid/SuperToken.sol";
import {
    SuperTokenV1Library
} from "../../../contracts/apps/SuperTokenV1Library.sol";

contract InstantDistributionAgreementV1IntegrationTest is FoundrySuperfluidTester {
    using SuperTokenV1Library for SuperToken;

    constructor() FoundrySuperfluidTester(3) {}

    function testAlice2Bob(
        uint32 indexId,
        uint32 units,
        uint32 newIndexValue
    ) public {
        vm.assume(units > 0);

        bool exist;
        uint128 indexValue;
        uint128 totalUnitsApproved;
        uint128 totalUnitsPending;

        // alice creates index
        vm.startPrank(alice);
        superToken.createIndex(indexId);
        superToken.updateSubscriptionUnits(indexId, bob, units);
        vm.stopPrank();
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = superToken
            .getIndex(alice, indexId);
        assertTrue(exist, "IDAv1.t: createIndex | index does not exist");
        assertEq(indexValue, 0, "IDAv1.t: createIndex | indexValue != 0");
        assertEq(totalUnitsApproved, 0, "IDAv1.t: createIndex | totalUnitsApproved != 0");
        assertEq(totalUnitsPending, units, "IDAv1.t: createIndex | totalUnitsPending != units");

        // alice distributes
        vm.startPrank(alice);
        superToken.updateIndexValue(indexId, newIndexValue);
        vm.stopPrank();
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = superToken
            .getIndex(alice, indexId);
        assertTrue(exist, "IDAv1.t: updateIndexValue | index does not exist");
        assertEq(indexValue, newIndexValue, "IDAv1.t: updateIndexValue | indexValue != newIndexValue");
        assertEq(totalUnitsApproved, 0, "IDAv1.t: updateIndexValue | totalUnitsApproved != 0");
        assertEq(totalUnitsPending, units, "IDAv1.t: updateIndexValue | totalUnitsPending != units");
        // bob subscribes to alice
        uint256 bobBalance1 = superToken.balanceOf(bob);
        vm.startPrank(bob);
        superToken.approveSubscription(alice, indexId);
        vm.stopPrank();
        uint256 bobBalance2 = superToken.balanceOf(bob);
        assertEq(
            bobBalance2 - bobBalance1,
            uint256(units) * uint256(newIndexValue),
            "IDAv1.t: approveSubscription | bobBalance2 - bobBalance1 != units * newIndexValue"
        );
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = superToken
            .getIndex(alice, indexId);
        assertTrue(exist, "IDAv1.t: approveSubscription | index does not exist");
        assertEq(indexValue, newIndexValue, "IDAv1.t: approveSubscription | indexValue != newIndexValue");
        assertEq(totalUnitsApproved, units, "IDAv1.t: approveSubscription | totalUnitsApproved != units");
        assertEq(totalUnitsPending, 0, "IDAv1.t: approveSubscription | totalUnitsPending != 0");

        _assertGlobalInvariants();
    }

    function testRevertMaxNumberOFSubscriptionsASubscriberCanHave() public {
        uint32 maxNumSubs = sf.ida.MAX_NUM_SUBSCRIPTIONS();
        
        for (uint256 i; i <maxNumSubs; ++i) {
            vm.startPrank(alice);
            superToken.createIndex(uint32(i));
            superToken.updateSubscriptionUnits(uint32(i), bob, 1);
            vm.stopPrank();

            vm.startPrank(bob);
            superToken.approveSubscription(alice, uint32(i));
            vm.stopPrank();
        }

        (,uint32[] memory indexIds,) = superToken.listSubscriptions(bob);
        assertEq(indexIds.length, maxNumSubs, "IDAv1.t: subscriptions length mismatch");

        vm.startPrank(alice);
        superToken.createIndex(uint32(maxNumSubs));
        superToken.updateSubscriptionUnits(uint32(maxNumSubs), bob, 1);
        vm.stopPrank();

        vm.startPrank(bob);
        vm.expectRevert("SlotBitmap out of bound");
        superToken.approveSubscription(alice, uint32(maxNumSubs));
        vm.stopPrank();
    }
}
