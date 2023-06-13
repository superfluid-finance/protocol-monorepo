// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "forge-std/console.sol";
import "../FoundrySuperfluidTester.sol";
import { ISuperApp, ISuperfluid } from "../../../contracts/interfaces/superfluid/ISuperfluid.sol";

// app to be tested
import { FlowSplitter } from "./FlowSplitter.sol";

interface IStreamHandler {
    function createFlow(address sender, uint32 flowRate) external;
    function updateFlow(address sender, uint32 flowRate) external;
    function deleteFlow(address sender) external;
}

contract Handler is IStreamHandler, Test {

    using SuperTokenV1Library for ISuperToken;

    address public superAppAddress;
    ISuperToken public superToken;

    // counters

    uint256 public _createFlowCallCount;
    uint256 public _skipCreateFlowCount;
    uint256 public _updateFlowCallCount;
    uint256 public _skipUpdateFlowCount;
    uint256 public _deleteFlowCallCount;
    uint256 public _skipDeleteFlowCount;


    constructor(address _superAppAddress, ISuperToken _superToken) {
        superAppAddress = _superAppAddress;
        superToken = _superToken;
    }

    function createFlow(address sender, uint32 flowRate) external override {
        int96 _flowRate = int96(uint96(flowRate));
        vm.startPrank(sender);
        if(superToken.getFlowRate(sender, superAppAddress) == 0) {
            _createFlowCallCount++;
            superToken.createFlow(superAppAddress, _flowRate);
        } else {
            _skipCreateFlowCount++;
        }

        vm.stopPrank();
    }

    function updateFlow(address sender, uint32 flowRate) external override {
        int96 _flowRate = int96(uint96(flowRate));
        vm.startPrank(sender);
        if(superToken.getFlowRate(sender, superAppAddress) > 0) {
            _updateFlowCallCount++;
            superToken.updateFlow(superAppAddress, _flowRate);
        } else {
            _skipUpdateFlowCount++;
        }
        vm.stopPrank();
    }

    function deleteFlow(address sender) external override {
        vm.startPrank(sender);
        if(superToken.getFlowRate(sender, superAppAddress) > 0) {
            _deleteFlowCallCount++;
            superToken.deleteFlow(sender, superAppAddress);
        } else {
            _skipDeleteFlowCount++;
        }
        vm.stopPrank();
    }

    function printCounters() external view {
        console.log("createFlowCallCount: %s", _createFlowCallCount);
        console.log("skipCreateFlowCount: %s", _skipCreateFlowCount);
        console.log("updateFlowCallCount: %s", _updateFlowCallCount);
        console.log("skipUpdateFlowCount: %s", _skipUpdateFlowCount);
        console.log("deleteFlowCallCount: %s", _deleteFlowCallCount);
        console.log("skipDeleteFlowCount: %s", _skipDeleteFlowCount);
    }

}

contract SuperAppInvariants is Test {

    ISuperfluid public host;
    ISuperApp public superApp;
    Handler public handler;


    function invariant_AppNeverJailed() public InitializeTests {
        assertTrue(!host.isAppJailed(superApp));
    }

    function invariant_AppRegistered() external InitializeTests {
        assertTrue(host.isApp(superApp));
    }

    function invariant_print() public view {
        handler.printCounters();
    }


    modifier InitializeTests() {
        if (address(superApp) == address(0)) revert("SuperAppTesterBase: no super app set");
        if (address(host) == address(0)) revert("SuperAppTesterBase: no host set");
        _;
    }

}

contract SuperAppTest is FoundrySuperfluidTester(10), SuperAppInvariants {

    // @notice: Very dependable on superApp utility
    int256 constant public MIN_FLOW_RATE = 1000;

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

        handler = new Handler(address(superApp), superToken);
        targetContract(address(handler));

        bytes4[] memory selectors = new bytes4[](3);
        selectors[0] = handler.createFlow.selector;
        selectors[1] = handler.updateFlow.selector;
        selectors[2] = handler.deleteFlow.selector;


        targetSelector(FuzzSelector({addr: address(handler), selectors: selectors}));

        for(uint256 i = 0; i < TEST_ACCOUNTS.length; i++) {
            targetSender(TEST_ACCOUNTS[i]);
        }

    }

}
