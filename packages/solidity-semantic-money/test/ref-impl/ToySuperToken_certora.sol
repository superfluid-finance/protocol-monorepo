// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "../../src/ref-impl/ToySuperToken.sol";


contract ToySuperTokenPoolCertora is ToySuperTokenPool {
    constructor (address admin) ToySuperTokenPool(admin) {}
}

contract ToySuperTokenCertora is ToySuperToken {
    mapping (address pool => FlowRate flowRate) private _poolFlowRates;

    function _setPDPIndex(address pool, PDPoolIndex memory p) internal virtual override {
        ToySuperToken._setPDPIndex(pool, p);
        _poolFlowRates[pool] = ToySuperTokenPool(pool).getDistributionFlowRate();
    }

}
