// SPDX-License-Identifier: MIT
/* solhint-disable private-vars-leading-underscore*/
pragma solidity >= 0.6.0;

library AppHelper {

    uint constant public BEFORE_AGREEMENT_CREATED_NOOP = 1 << 1;
    uint constant public AFTER_AGREEMENT_CREATED_NOOP = 1 << 2;
    uint constant public BEFORE_AGREEMENT_UPDATED_NOOP = 1 << 3;
    uint constant public AFTER_AGREEMENT_UPDATED_NOOP = 1 << 4;
    uint constant public BEFORE_AGREEMENT_TERMINATED_NOOP = 1 << 8;

}
