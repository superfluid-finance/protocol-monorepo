// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import {HotFuzzBase, SuperfluidTester} from "../HotFuzzBase.sol";
import {ISuperfluidPool} from
    "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidPool.sol";

abstract contract GDAHotFuzzMixin is HotFuzzBase {
    ISuperfluidPool[] public pools;

    function getRandomPool(uint256 input) public view returns (ISuperfluidPool pool) {
        if (pools.length > 0) {
            pool = pools[input % (pools.length - 1)];
        }
    }

    function createPool(uint8 a) public {
        (SuperfluidTester tester) = _getOneTester(a);
        ISuperfluidPool pool = tester.createPool(address(tester));
        _addPool(pool);
    }

    function maybeConnectPool(bool doConnect, uint8 a, uint8 b) public {
        (SuperfluidTester tester) = _getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);
        if (doConnect) {
            tester.connectPool(pool);
        } else {
            tester.disconnectPool(pool);
        }
    }

    function distributeToPool(uint8 a, uint8 b, uint128 requestedAmount) public {
        (SuperfluidTester tester) = _getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.distributeToPool(pool, address(tester), requestedAmount);
    }

    function distributeFlow(uint8 a, uint8 b, uint8 c, int96 flowRate) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(c);

        testerA.distributeFlow(pool, address(testerB), flowRate);
    }

    function updateMemberUnits(uint8 a, uint8 b, uint128 units) public {
        (SuperfluidTester tester) = _getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.updateMemberUnits(pool, address(tester), units);
    }

    function poolTransfer(uint8 a, uint8 b, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.transfer(pool, address(testerB), amount);
    }

    function poolTransferFrom(uint8 a, uint8 b, uint8 c, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        (SuperfluidTester tester) = _getOneTester(c);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.transferFrom(pool, address(tester), address(testerB), amount);
    }

    function poolIncreaseAllowance(uint8 a, uint8 b, uint256 addedValue) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.increaseAllowance(pool, address(testerB), addedValue);
    }

    function poolDecreaseAllowance(uint8 a, uint8 b, uint256 subtractedValue) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.decreaseAllowance(pool, address(testerB), subtractedValue);
    }

    function poolApprove(uint8 a, uint8 b, uint256 amount) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.approve(pool, address(testerB), amount);
    }

    function claimAll(uint8 a, uint8 b) public {
        (SuperfluidTester tester) = _getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.claimAll(pool);
    }

    function claimAllForMember(uint8 a, uint8 b) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = _getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(b);

        testerA.claimAll(pool, address(testerB));
    }

    function _addPool(ISuperfluidPool pool) internal {
        pools.push(pool);
        _addAccount(address(pool));
    }
}

contract GDAHotFuzz is HotFuzzBase(10), GDAHotFuzzMixin {
    uint256 public constant NUM_POOLS = 3;

    constructor() {
        _initTesters();

        for (uint256 i; i < NUM_POOLS; i++) {
            (SuperfluidTester tester) = _getOneTester(uint8(i));
            // create a pool breaks
            ISuperfluidPool pool = tester.createPool(address(tester));
            _addPool(pool);
        }
    }
}
