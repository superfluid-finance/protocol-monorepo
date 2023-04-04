// SPDX-License-Identifier: UNLICENSED
pragma solidity 0.8.19;

import "../../src/ref-impl/ToySuperToken.sol";


contract ToySuperTokenPoolCertora is ToySuperTokenPool {
    constructor (address admin) ToySuperTokenPool(admin) {}
}

contract ToySuperTokenCertora is ToySuperToken {

    event SET_UINDEX(address owner, FlowRate r0, FlowRate r1);

    /* function _setUIndex(bytes memory eff, address owner, BasicParticle memory p) */
    /*     internal virtual override */
    /* { */
    /*     FlowRate r0 = uIndexes[owner].flow_rate; */
    /*     super._setUIndex(eff, owner, p); */
    /*     FlowRate r1 = uIndexes[owner].flow_rate; */
    /*     emit SET_UINDEX(owner, r0, r1); */
    /* } */

    /* function _setPDPIndex(bytes memory /\*eff*\/, address pool, PDPoolIndex memory p) */
    /*     internal virtual override */
    /* { */
    /*     assert(ISuperTokenPool(pool).operatorSetIndex(p)); */
    /* } */

}
