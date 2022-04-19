// SPDX-License-Identifier: AGPLv3
// solhint-disable reason-string
pragma solidity >= 0.8.0;

import "./BaseFuzzer.sol";

abstract contract AbstractIDAFuzzer is AbstractBaseFuzzer {

    uint32 private constant MAX_NUM_INDICES = 3;

    function setupIndex(uint8 a, uint8 b, uint32 indexId, uint128 units) public {
        indexId = indexId % MAX_NUM_INDICES;

        (SuperfluidTester testerA, SuperfluidTester testerB) = getTester2(a, b);

        (bool exists,,,) = ida().getIndex(superToken(), address(testerA), indexId);
        if (!exists) {
            testerA.createIndex(indexId);
        }
        testerA.updateSubscriptionUnits(indexId, address(testerB), units);
    }

    function distributeIfIndexExists(uint8 a, uint32 indexId, uint256 amount) public {
        indexId = indexId % MAX_NUM_INDICES;
        (SuperfluidTester testerA) = getTester1(a);

        (bool exists,,,) = ida().getIndex(superToken(), address(testerA), indexId);
        if (exists) {
            (uint256 actualAmount, ) = ida().calculateDistribution(superToken(), address(testerA), indexId, amount);
            testerA.distribute(indexId, amount);
            int256 a1 = superTokenBalanceOfNow(address(testerA));
            int256 a2 = superTokenBalanceOfNow(address(testerA));
            assert(a1 - a2 == int256(actualAmount));
        }
    }

    function updateSubscriptionUnits(uint8 a, uint8 b, uint32 indexId, uint128 units) public {
        require(units > 0);

        indexId = indexId % MAX_NUM_INDICES;
        (SuperfluidTester testerA, SuperfluidTester testerB) = getTester2(a, b);

        testerA.updateSubscriptionUnits(indexId, address(testerB), units);
        bool exist;
        uint128 unitsActual;
        (exist, , unitsActual, ) = ida().getSubscription(superToken(), address(testerA), indexId, address(testerB));
        assert(exist == true);
        assert(unitsActual == units);
    }

    function approveSubscriptionUnits(uint8 a, uint8 b, uint32 indexId) public {
        indexId = indexId % MAX_NUM_INDICES;

        (SuperfluidTester testerA, SuperfluidTester testerB) = getTester2(a, b);
        bool exist;
        bool approved;
        ida().getSubscription(superToken(), address(testerA), indexId, address(testerB));
        testerB.approveSubscription(address(testerA), indexId);
        (exist, approved, , ) = ida().getSubscription(superToken(), address(testerA), indexId, address(testerB));
        //assert(exist1 == exist2);
        assert(exist == true);
        assert(approved == true);
    }

}

// solhint-disable-next-line no-empty-blocks
contract IDAFuzzer is AbstractIDAFuzzer, BaseFuzzer { }
