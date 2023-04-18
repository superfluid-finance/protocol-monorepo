// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { console } from "forge-std/console.sol";
import {
    GeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";
import {
    ISuperfluidToken
} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import {
    ISuperTokenPool,
    SuperTokenPool
} from "../../../contracts/superfluid/SuperTokenPool.sol";

contract GeneralDistributionAgreementV1Test is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(2) {}

    SuperTokenPool public pool;
    uint256 public liquidationPeriod;

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = SuperTokenPool(address(sf.gda.createPool(alice, superToken)));
        (liquidationPeriod, ) = sf.governance.getPPPConfig(sf.host, superToken);
    }

    function helper_Connect_Pool(ISuperTokenPool _pool) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeWithSelector(
                IGeneralDistributionAgreementV1.connectPool.selector,
                _pool,
                ""
            ),
            new bytes(0)
        );
    }

    function helper_Disonnect_Pool(ISuperTokenPool _pool) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.disconnectPool, (_pool, "")),
            new bytes(0)
        );
    }

    function helper_Disconnect_Pool(ISuperTokenPool _pool) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(sf.gda.disconnectPool, (_pool, new bytes(0))),
            new bytes(0)
        );
    }

    function _helper_Distribute(
        ISuperfluidToken _superToken,
        SuperTokenPool _pool,
        uint256 requestedAmount
    ) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(
                sf.gda.distribute,
                (_superToken, _pool, requestedAmount, new bytes(0))
            ),
            new bytes(0)
        );
    }

    function _helper_Distribute_Flow(
        ISuperfluidToken _superToken,
        SuperTokenPool _pool,
        int96 requestedFlowRate
    ) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(
                sf.gda.distributeFlow,
                (_superToken, _pool, requestedFlowRate, new bytes(0))
            ),
            new bytes(0)
        );
    }

    function test_Create_Pool() public {
        vm.prank(alice);
        SuperTokenPool localPool = SuperTokenPool(
            address(sf.gda.createPool(alice, superToken))
        );
    }

    function test_Connect_Pool() public {
        vm.startPrank(bob);
        helper_Connect_Pool(pool);
        vm.stopPrank();
    }

    function test_Disconnect_Pool() public {
        vm.startPrank(bob);
        helper_Connect_Pool(pool);
        helper_Disonnect_Pool(pool);
        vm.stopPrank();
    }

    function test_Realtime_Balance_of_Empty() public {
        (int256 balance, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            bob,
            block.timestamp
        );
        assertEq(balance, 0);
    }

    function test_Distribute_With_No_Connections() public {
        vm.startPrank(alice);
        _helper_Distribute(superToken, pool, 100);
        vm.stopPrank();
    }

    function test_Distribute_To_Connected_Member() public {
        vm.startPrank(bob);
        helper_Connect_Pool(pool);
        vm.stopPrank();

        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helper_Distribute(superToken, pool, 100);
        vm.stopPrank();
        (int256 rtb, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            bob,
            block.timestamp
        );
        assert(rtb > 0);
    }

    struct DistributionData {
        uint256 timeDelta;
        int256[] rtbsBefore;
        int96[] perMemberFlowRate;
    }

    struct Allocation {
        address member;
        int128 memberUnits;
    }

    function helper_Update_Units_And_Assert(
        address poolAdmin,
        Allocation[10] memory allocation
    ) public {
        vm.startPrank(poolAdmin);
        // allocate units to members
        for (uint i; i < allocation.length; i++) {
            pool.updateMember(allocation[i].member, allocation[i].memberUnits);

            // assert per member units were correctly set
            assertEq(
                pool.getUnits(allocation[i].member),
                allocation[i].memberUnits,
                "units incorrectly set"
            );
        }
        vm.stopPrank();
    }

    function helper_Distribute_Flow_And_Assert(
        ISuperfluidToken desiredToken,
        SuperTokenPool to,
        address poolAdmin,
        int96 requestedDistributionFlowRate
    ) public returns (int96 actualDistributionFlowRate) {
        vm.startPrank(poolAdmin);

        // actualDistributionFlowRate provided given number of units and requestedFlowRate
        actualDistributionFlowRate = sf.gda.getFlowDistributionActualFlowRate(
            desiredToken,
            poolAdmin,
            to,
            requestedDistributionFlowRate
        );

        // distribute flow of requestedDistributionFlowRate to 10 members with perMemberUnits units each
        _helper_Distribute_Flow(
            desiredToken,
            to,
            requestedDistributionFlowRate
        );

        int96 poolFlowRateAfter = to.getDistributionFlowRate();

        // assert flow rate after is the actualDistributionFlowRate based on helper function
        assertEq(
            poolFlowRateAfter,
            actualDistributionFlowRate,
            "pool flow rate after distribution !="
        );

        vm.stopPrank();
    }

    function helper_Connect_To_Pool_And_Assert(
        Allocation[10] memory allocation
    ) public {
        for (uint i; i < allocation.length; i++) {
            // assert that no members connected
            bool isConnectedBefore = sf.gda.isMemberConnected(
                superToken,
                address(pool),
                allocation[i].member
            );
            assertFalse(isConnectedBefore);

            // connect if unconnected
            if (!isConnectedBefore) {
                // all members connect to the pool
                vm.startPrank(allocation[i].member);
                helper_Connect_Pool(pool);
                vm.stopPrank();

                // assert all members are connected
                bool isConnectedAfter = sf.gda.isMemberConnected(
                    superToken,
                    address(pool),
                    allocation[i].member
                );
                assertTrue(isConnectedAfter, "isConnectedAfter");
            }
        }
    }

    function helper_Warp_Time_And_Assert_Balances(
        Allocation[10] memory allocation,
        int256 distributorRTBBefore,
        int96 actualDistributionFlowRate,
        uint256 timeDelta
    ) public {
        DistributionData memory dd;
        dd.timeDelta = timeDelta;
        dd.rtbsBefore = new int256[](10);
        dd.perMemberFlowRate = new int96[](10);

        for (uint i; i < allocation.length; i++) {
            // get the rtb for all members before
            (int256 rtb, , ) = superToken.realtimeBalanceOf(
                allocation[i].member,
                block.timestamp
            );
            dd.rtbsBefore[i] = rtb;

            // get the per member flow rate based on their units and the actualDistributionFlowRate for the pool
            int96 memberFlowRate = pool.getMemberFlowRate(allocation[i].member);
            dd.perMemberFlowRate[i] = memberFlowRate;
        }
        // memberFlowRate is correct

        // move the time forwards by timeDelta
        vm.warp(block.timestamp + dd.timeDelta);

        for (uint i; i < allocation.length; i++) {
            // get rtb after timeDelta
            (int256 rtbAfter, , ) = superToken.realtimeBalanceOf(
                allocation[i].member,
                block.timestamp
            );

            // assert that the rtb after is equal the to rtb before + the amount flowed
            assertEq(
                rtbAfter,
                dd.rtbsBefore[i] +
                    int256(dd.timeDelta) *
                    int256(dd.perMemberFlowRate[i]),
                "RTB of member != rtbBefore + amount flowed"
            );
        }

        // get rtb of the distributor after time delta
        (int256 distributorRTBAfter, , ) = superToken.realtimeBalanceOf(
            alice,
            block.timestamp
        );
        // assert that the rtb before less the amount flowed is equal to the rtb after time delta
        assertEq(
            distributorRTBBefore -
                actualDistributionFlowRate *
                int96(int256(dd.timeDelta)),
            distributorRTBAfter,
            "distributor RTB after time warp !="
        );
    }

    function test_Distribute_Flow_To_Connected_Member() public {
        address[10] memory ACCOUNTS = [
            0x30B125d5Fc58c1b8E3cCB2F1C71a1Cc847f024eE,
            0xc41876DAB61De145093b6aA87417326B24Ae4ECD,
            0x04A4417CBc87009Ba69a730756F4367436E41a5F,
            0x8A29B917d9f652d82D757fF7c24048AC8A1eD185,
            0x8C53b907c856ff11708a8C4cFef4F4643CAf8B8F,
            0x5fA51030cA48D55fAd71e9D22A31A86b777A0933,
            0xd66E40b0c30595bEc72153B502aC1E0c4785991B,
            0x5a6758aEf70A5Df011dB853014cAabC34c6a144d,
            0x1965678C67dA12B2B2D2517886258C59996Cd540,
            0x718FB07d52Ff331CE8004955c26dF1A1c357b4c8
        ];
        int128 perMemberUnits = 4206933;
        int96 requestedDistributionFlowRate = 420693300;

        Allocation[10] memory allocation;
        for (uint i; i < ACCOUNTS.length; i++) {
            allocation[i].member = ACCOUNTS[i];
            allocation[i].memberUnits = perMemberUnits % int128(int256(i + 1));
        }
        helper_Update_Units_And_Assert(alice, allocation);

        // no distribution flow rate initially
        int96 poolFlowRateBefore = pool.getDistributionFlowRate();
        assertEq(poolFlowRateBefore, 0);

        int96 actualDistributionFlowRate = helper_Distribute_Flow_And_Assert(
            superToken,
            pool,
            alice,
            requestedDistributionFlowRate
        );

        (int256 distributorRTBBefore, uint256 deposit, ) = superToken
            .realtimeBalanceOf(alice, block.timestamp);

        assertEq(
            deposit,
            uint256(uint96(actualDistributionFlowRate)) * liquidationPeriod
        );

        helper_Connect_To_Pool_And_Assert(allocation);

        helper_Warp_Time_And_Assert_Balances(
            allocation,
            distributorRTBBefore,
            actualDistributionFlowRate,
            2
        );

        // update units scenario

        for (uint i; i < ACCOUNTS.length; i++) {
            allocation[i].member = ACCOUNTS[i];
            allocation[i].memberUnits =
                perMemberUnits %
                int128(int256(i + 420));
        }
        helper_Update_Units_And_Assert(alice, allocation);

        actualDistributionFlowRate = helper_Distribute_Flow_And_Assert(
            superToken,
            pool,
            alice,
            4127346127
        );
        (distributorRTBBefore, deposit, ) = superToken.realtimeBalanceOf(
            alice,
            block.timestamp
        );

        assertEq(
            deposit,
            uint256(uint96(actualDistributionFlowRate)) * liquidationPeriod
        );

        helper_Warp_Time_And_Assert_Balances(
            allocation,
            distributorRTBBefore,
            actualDistributionFlowRate,
            0
        );
    }

    function test_Distribute_Flow_To_Unconnected_Member() public {
        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        pool.updateMember(carol, 1);
        _helper_Distribute_Flow(superToken, pool, 100);
        vm.stopPrank();
        vm.warp(block.timestamp + 1);
        (int256 rtb, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            bob,
            block.timestamp
        );
        assert(rtb == 0);
    }
}
