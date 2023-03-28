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
    constructor() FoundrySuperfluidTester(3) {}

    SuperTokenPool public pool;

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = SuperTokenPool(address(sf.gda.createPool(alice, superToken)));
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

    function test_Distribute_Flow_To_Connected_Member() public {
        vm.startPrank(bob);
        helper_Connect_Pool(pool);
        vm.stopPrank();

        vm.startPrank(carol);
        helper_Connect_Pool(pool);
        vm.stopPrank();

        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        pool.updateMember(carol, 1);
        _helper_Distribute_Flow(superToken, pool, 100);
        vm.stopPrank();
        vm.warp(block.timestamp + 1);
        (int256 bobRTB, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            bob,
            block.timestamp
        );
        assert(bobRTB == 50);

        (int256 carolRTB, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            carol,
            block.timestamp
        );
        assert(carolRTB == 50);
    }

    // function test_Distribute_Flow_To_Connected_Member_Then_Distribute_Zero_Flow_Rate() public {
    //     vm.startPrank(bob);
    //     helper_Connect_Pool(pool);
    //     vm.stopPrank();

    //     vm.startPrank(carol);
    //     helper_Connect_Pool(pool);
    //     vm.stopPrank();

    //     vm.startPrank(alice);
    //     pool.updateMember(bob, 1);
    //     pool.updateMember(carol, 1);
    //     _helper_Distribute_Flow(superToken, pool, 100);
    //     vm.stopPrank();
    //     vm.warp(block.timestamp + 1);
    //     (int256 bobRTB, , ) = sf.gda.realtimeBalanceOf(
    //         superToken,
    //         bob,
    //         block.timestamp
    //     );
    //     assert(bobRTB == 50);

    //     (int256 carolRTB, , ) = sf.gda.realtimeBalanceOf(
    //         superToken,
    //         carol,
    //         block.timestamp
    //     );
    //     assert(carolRTB == 50);

    //     _helper_Distribute_Flow(superToken, pool, 0);

    //     (bobRTB, , ) = sf.gda.realtimeBalanceOf(
    //         superToken,
    //         bob,
    //         block.timestamp
    //     );
    //     assert(bobRTB == 0);

    //     (carolRTB, , ) = sf.gda.realtimeBalanceOf(
    //         superToken,
    //         carol,
    //         block.timestamp
    //     );
    //     assert(carolRTB == 0);
    // }

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
