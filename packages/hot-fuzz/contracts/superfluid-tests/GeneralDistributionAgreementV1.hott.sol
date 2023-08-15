// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import {HotFuzzBase, SuperfluidTester} from "../HotFuzzBase.sol";
import {ISuperfluidPool} from
    "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluidPool.sol";

abstract contract GDAHotFuzzMixin is HotFuzzBase {
    ISuperfluidPool[] public pools;

    function getRandomPool(uint256 input) public view returns (ISuperfluidPool) {
        if (pools.length > 0) {
            return pools[input % (pools.length - 1)];
        }
    }

    function createPool(uint8 a) public {
        (SuperfluidTester tester) = getOneTester(a);

        pools.push(tester.createPool(address(tester)));
    }

    function connectPool(uint8 a, uint8 b) public {
        (SuperfluidTester tester) = getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.connectPool(pool);
    }

    function disconnectPool(uint8 a, uint8 b) public {
        (SuperfluidTester tester) = getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.disconnectPool(pool);
    }

    function distributeToPool(uint8 a, uint8 b, uint128 requestedAmount) public {
        (SuperfluidTester tester) = getOneTester(a);
        ISuperfluidPool pool = getRandomPool(b);

        tester.distributeToPool(pool, address(tester), requestedAmount);
    }

    function distributeFlow(uint8 a, uint8 b, uint8 c, int96 flowRate) public {
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);
        ISuperfluidPool pool = getRandomPool(c);

        testerA.distributeFlow(pool, address(testerB), flowRate);
    }
}

contract GDAHotFuzz is HotFuzzBase(10), GDAHotFuzzMixin {

    uint256 public constant NUM_POOLS = 3;
    constructor() {
        initTesters();

        for (uint256 i; i < NUM_POOLS; i++) {
            (SuperfluidTester tester) = getOneTester(uint8(i));
            // create a pool breaks
            ISuperfluidPool pool = tester.createPool(address(tester));
            pools.push(pool);
        }
    }
    
}
