// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";

abstract contract CFAHotFuzzMixin is HotFuzzBase {
    function createFlow(uint8 a, uint8 b, int64 flowRate) public {
        require(flowRate > 0);
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.flow(address(testerB), int96(flowRate));
    }

    function deleteFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.flow(address(testerB), 0);
    }

    function liquidateFlow(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        bool success = testerA.flow(address(testerB));
        assert(success);
    }

    function setFlowPermissions(
        uint8 a,
        uint8 b,
        bool allowCreate,
        bool allowUpdate,
        bool allowDelete,
        int96 flowRateAllowance
    ) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.setFlowPermissions(address(testerB), allowCreate, allowUpdate, allowDelete, flowRateAllowance);
    }

    function setMaxFlowPermissions(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.setMaxFlowPermissions(address(testerB));
    }

    function revokeFlowPermissions(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.revokeFlowPermissions(address(testerB));
    }

    function increaseFlowRateAllowance(uint8 a, uint8 b, int96 addedFlowRateAllowance) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.increaseFlowRateAllowance(address(testerB), addedFlowRateAllowance);
    }

    function decreaseFlowRateAllowance(uint8 a, uint8 b, int96 subtractedFlowRateAllowance) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.decreaseFlowRateAllowance(address(testerB), subtractedFlowRateAllowance);
    }

    function increaseFlowRateAllowanceWithPermissions(
        uint8 a,
        uint8 b,
        uint8 permissionsToAdd,
        int96 addedFlowRateAllowance
    ) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.increaseFlowRateAllowanceWithPermissions(address(testerB), permissionsToAdd, addedFlowRateAllowance);
    }

    function decreaseFlowRateAllowanceWithPermissions(
        uint8 a,
        uint8 b,
        uint8 permissionsToRemove,
        int96 subtractedFlowRateAllowance
    ) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);

        testerA.decreaseFlowRateAllowanceWithPermissions(
            address(testerB), permissionsToRemove, subtractedFlowRateAllowance
        );
    }
}

contract CFAHotFuzz is CFAHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        _initTesters();
    }
}
