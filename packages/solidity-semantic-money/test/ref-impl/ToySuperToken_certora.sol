// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "../../src/ref-impl/ToySuperToken.sol";


contract ToySuperTokenPoolCertora is ToySuperTokenPool {
    constructor (address admin) ToySuperTokenPool(admin) {}
}

contract ToySuperTokenCertora is ToySuperToken {
}
