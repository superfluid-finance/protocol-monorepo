// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "@superfluid-finance/solidity-semantic-money/src/ref-impl/ToySuperToken.sol";


contract ToySuperTokenPoolCertora is ToySuperTokenPool {
    constructor (address admin) ToySuperTokenPool(admin) {}
}

contract ToySuperTokenCertora is ToySuperToken {
    using EnumerableSet for EnumerableSet.AddressSet;

    function distribute(address from, uint poolId, Value reqAmount) external
        returns (bool success, Value actualAmount)
    {
        require(poolId < _pools.length(), "Invalid poolId");
        return distribute(from, ISuperTokenPool(_pools.at(poolId)), reqAmount);
    }

    function distributeFlow(address from, uint poolId, FlowId flowId, FlowRate reqFlowRate) external
        returns (bool success, FlowRate actualFlowRate)
    {
        require(poolId < _pools.length(), "Invalid poolId");
        return distributeFlow(from, ISuperTokenPool(_pools.at(poolId)), flowId, reqFlowRate);
    }
}
