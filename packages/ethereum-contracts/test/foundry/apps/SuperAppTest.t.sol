// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "forge-std/console.sol";
import "../FoundrySuperfluidTester.sol";
import { ISuperApp, ISuperfluid } from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";

// app to be tested
import { FlowSplitter } from "./FlowSplitter.sol";

contract SuperAppInvariants is Test {

    ISuperfluid public host;
    ISuperApp public superApp;

    /// forge-config: default.invariant.runs = 10
    /// forge-config: default.invariant.depth = 2
    /// forge-config: default.invariant.fail-on-revert = true
    function invariant_AppNeverJailed() public InitializeTests {
        assertTrue(!host.isAppJailed(superApp));
    }

    /// forge-config: default.invariant.runs = 10
    /// forge-config: default.invariant.depth = 2
    /// forge-config: default.invariant.fail-on-revert = true
    function invariant_AppRegistered() public InitializeTests {
        assertTrue(host.isApp(superApp));
    }


    modifier InitializeTests() {
        if (address(superApp) == address(0)) revert("SuperAppTesterBase: no super app set");
        if (address(host) == address(0)) revert("SuperAppTesterBase: no host set");
        _;
    }

}


contract SuperAppTest is FoundrySuperfluidTester, SuperAppInvariants {

    using SuperTokenV1Library for ISuperToken;

    // @notice: Very dependable on superApp utility
    int256 constant public MIN_FLOW_RATE = 1000;

    address public superAppAddress;

    constructor() FoundrySuperfluidTester(3) { }

    function setUp() public override {
        super.setUp();
        host = sf.host;
        // create super app
        superApp = ISuperApp(address(new FlowSplitter(
            address(0x51),
            address(0x52),
            300,
            superToken,
            sf.host
        )));

        superAppAddress = address(superApp);
    }

    /// forge-config: default.fuzz.runs = 100
    function testCreateFlowToSuperApp(int96 flowRate) public {
        flowRate = int96(bound(flowRate, MIN_FLOW_RATE, int96(uint96(type(uint32).max))));
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, flowRate);
        assertEq(
            superToken.getFlowRate(alice, superAppAddress), flowRate, "SuperAppTester: createFlow | flowRate incorrect"
        );
        vm.stopPrank();
    }

    function testUpdateFlowToSuperApp(int96 flowRate, int96 updatedFlowRate) public {
        flowRate = int96(bound(flowRate, MIN_FLOW_RATE, int96(uint96(type(uint32).max))));
        updatedFlowRate = int96(bound(flowRate, MIN_FLOW_RATE, int96(uint96(type(uint32).max))));
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, flowRate);
        assertEq(
            superToken.getFlowRate(alice, superAppAddress), flowRate, "SuperAppTester: updateFlow | flowRate incorrect"
        );
        superToken.updateFlow(superAppAddress, updatedFlowRate);
        assertEq(
            superToken.getFlowRate(alice, superAppAddress),
            updatedFlowRate,
            "SuperAppBase: updateFlow | updatedFlowRate incorrect"
        );
        vm.stopPrank();
    }

    // test delete flow
    function testDeleteFlowToSuperApp(int96 flowRate) public {
        flowRate = int96(bound(flowRate, MIN_FLOW_RATE, int96(uint96(type(uint32).max))));
        vm.startPrank(alice);
        superToken.createFlow(superAppAddress, flowRate);
        superToken.deleteFlow(alice, superAppAddress);
        assertEq(superToken.getFlowRate(alice, superAppAddress), 0, "SuperAppTester: deleteFlow | flowRate incorrect");
        vm.stopPrank();
    }



}
