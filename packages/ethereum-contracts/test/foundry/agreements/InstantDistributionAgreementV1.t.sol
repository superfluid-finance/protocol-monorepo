// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";

contract InstantDistributionAgreementV1Anvil is FoundrySuperfluidTester {
    using IDAv1Library for IDAv1Library.InitData;

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
        sf.idaLib.createIndex(superToken, indexId);
        sf.idaLib.updateSubscriptionUnits(superToken, indexId, bob, units);
        vm.stopPrank();
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = sf
            .idaLib
            .getIndex(superToken, alice, indexId);
        assertTrue(exist, "IDAv1.t: createIndex | index does not exist");
        assertEq(indexValue, 0, "IDAv1.t: createIndex | indexValue != 0");
        assertEq(
            totalUnitsApproved,
            0,
            "IDAv1.t: createIndex | totalUnitsApproved != 0"
        );
        assertEq(
            totalUnitsPending,
            units,
            "IDAv1.t: createIndex | totalUnitsPending != units"
        );

        // alice distributes
        vm.startPrank(alice);
        sf.idaLib.updateIndexValue(superToken, indexId, newIndexValue);
        vm.stopPrank();
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = sf
            .idaLib
            .getIndex(superToken, alice, indexId);
        assertTrue(exist, "IDAv1.t: updateIndexValue | index does not exist");
        assertEq(
            indexValue,
            newIndexValue,
            "IDAv1.t: updateIndexValue | indexValue != newIndexValue"
        );
        assertEq(
            totalUnitsApproved,
            0,
            "IDAv1.t: updateIndexValue | totalUnitsApproved != 0"
        );
        assertEq(
            totalUnitsPending,
            units,
            "IDAv1.t: updateIndexValue | totalUnitsPending != units"
        );

        // bob subscribes to alice
        uint256 bobBalance1 = superToken.balanceOf(bob);
        vm.startPrank(bob);
        sf.idaLib.approveSubscription(superToken, alice, indexId);
        vm.stopPrank();
        uint256 bobBalance2 = superToken.balanceOf(bob);
        assertEq(
            bobBalance2 - bobBalance1,
            uint256(units) * uint256(newIndexValue),
            "IDAv1.t: approveSubscription | bobBalance2 - bobBalance1 != units * newIndexValue"
        );
        (exist, indexValue, totalUnitsApproved, totalUnitsPending) = sf
            .idaLib
            .getIndex(superToken, alice, indexId);
        assertTrue(exist, "IDAv1.t: approveSubscription | index does not exist");
        assertEq(indexValue, newIndexValue, "IDAv1.t: approveSubscription | indexValue != newIndexValue");
        assertEq(totalUnitsApproved, units, "IDAv1.t: approveSubscription | totalUnitsApproved != units");
        assertEq(totalUnitsPending, 0, "IDAv1.t: approveSubscription | totalUnitsPending != 0");

        _assert_Global_Invariants();
    }
}
