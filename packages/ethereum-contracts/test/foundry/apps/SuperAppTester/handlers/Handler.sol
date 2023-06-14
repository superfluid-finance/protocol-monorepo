// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import "forge-std/Test.sol";
import "./IStreamHandler.sol";
import { SuperTokenV1Library, ISuperToken } from "../../../../../contracts/apps/SuperTokenV1Library.sol";

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
