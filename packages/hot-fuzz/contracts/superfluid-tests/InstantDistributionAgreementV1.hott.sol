// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "../HotFuzzBase.sol";


abstract contract IDAHotFuzzMixin is HotFuzzBase {
    uint32 private constant MAX_NUM_INDICES = 3;

    function setupIndex(uint8 a, uint8 b, uint32 indexId, uint128 units) public {
        indexId = indexId % MAX_NUM_INDICES;

        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);

        (bool exists,,,) = sf.ida.getIndex(superToken, address(testerA), indexId);
        if (!exists) {
            testerA.createIndex(indexId);
        }
        testerA.updateSubscriptionUnits(indexId, address(testerB), units);
    }

    function distributeIfIndexExists(uint8 a, uint32 indexId, uint256 amount) public {
        indexId = indexId % MAX_NUM_INDICES;
        (SuperfluidTester testerA) = getOneTester(a);

        (bool exists,,,) = sf.ida.getIndex(superToken, address(testerA), indexId);
        if (exists) {
            (uint256 actualAmount, ) = sf.ida.calculateDistribution(superToken, address(testerA), indexId, amount);
            testerA.distribute(indexId, amount);
            int256 a1 = superTokenBalanceOfNow(address(testerA));
            int256 a2 = superTokenBalanceOfNow(address(testerA));
            assert(a1 - a2 == int256(actualAmount));
        }
    }

    function updateSubscriptionUnits(uint8 a, uint8 b, uint32 indexId, uint128 units) public {
        require(units > 0);

        indexId = indexId % MAX_NUM_INDICES;
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);

        testerA.updateSubscriptionUnits(indexId, address(testerB), units);
        bool exist;
        uint128 unitsActual;
        (exist, , unitsActual, ) = sf.ida.getSubscription(superToken, address(testerA), indexId, address(testerB));
        assert(exist == true);
        assert(unitsActual == units);
    }

    function approveSubscriptionUnits(uint8 a, uint8 b, uint32 indexId) public {
        indexId = indexId % MAX_NUM_INDICES;

        (SuperfluidTester testerA, SuperfluidTester testerB) = getTwoTesters(a, b);
        bool exist;
        bool approved;
        sf.ida.getSubscription(superToken, address(testerA), indexId, address(testerB));
        testerB.approveSubscription(address(testerA), indexId);
        (exist, approved, , ) = sf.ida.getSubscription(superToken, address(testerA), indexId, address(testerB));
        //assert(exist1 == exist2);
        assert(exist == true);
        assert(approved == true);
    }
}

contract IDAHotFuzz is IDAHotFuzzMixin {
    constructor() HotFuzzBase(10) {
        initTesters();
    }
}
