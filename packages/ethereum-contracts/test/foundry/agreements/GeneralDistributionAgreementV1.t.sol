// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "../FoundrySuperfluidTester.sol";
import { console } from "forge-std/console.sol";
import {
    FlowId,
    GeneralDistributionAgreementV1
} from "../../../contracts/agreements/GeneralDistributionAgreementV1.sol";
import {
    ISuperfluidToken
} from "../../../contracts/interfaces/superfluid/ISuperfluidToken.sol";
import {
    SuperTokenPool
} from "../../../contracts/superfluid/SuperTokenPool.sol";

contract GeneralDistributionAgreementV1Test is FoundrySuperfluidTester {
    constructor() FoundrySuperfluidTester(3) {}

    SuperTokenPool public pool;

    function setUp() public override {
        super.setUp();
        vm.prank(alice);
        pool = sf.gda.createPool(alice, superToken);
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
        FlowId flowId,
        int96 requestedFlowRate
    ) internal {
        sf.host.callAgreement(
            sf.gda,
            abi.encodeCall(
                sf.gda.distributeFlow,
                (_superToken, _pool, flowId, requestedFlowRate, new bytes(0))
            ),
            new bytes(0)
        );
    }

    function test_Create_Pool() public {
        vm.prank(alice);
        SuperTokenPool localPool = sf.gda.createPool(alice, superToken);
    }

    function test_Connect_Pool() public {
        vm.startPrank(bob);
        sf.gda.connectPool(pool);
        vm.stopPrank();
    }

    function test_Disconnect_Pool() public {
        vm.startPrank(bob);
        sf.gda.connectPool(pool);
        sf.gda.disconnectPool(pool);
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
        sf.gda.connectPool(pool);
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
        sf.gda.connectPool(pool);
        vm.stopPrank();

        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helper_Distribute_Flow(superToken, pool, FlowId.wrap(0), 100);
        vm.stopPrank();
        vm.warp(block.timestamp + 1);
        (int256 rtb, , ) = sf.gda.realtimeBalanceOf(
            superToken,
            bob,
            block.timestamp
        );
        assert(rtb == 100);
    }

    function test_Distribute_Flow_To_Unconnected_Member() public {
        vm.startPrank(alice);
        pool.updateMember(bob, 1);
        _helper_Distribute_Flow(superToken, pool, FlowId.wrap(0), 100);
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
