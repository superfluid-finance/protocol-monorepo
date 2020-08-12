// SPDX-License-Identifier: MIT
/* solhint-disable private-vars-leading-underscore*/
pragma solidity >= 0.6.0;

library AppHelper {

    uint constant public TYPE_APP_FINAL = 1 << 0;
    uint constant public TYPE_APP_SECOND = 1 << 1;

    uint constant public BEFORE_AGREEMENT_CREATED_NOOP = 1 << (32 + 0);
    uint constant public AFTER_AGREEMENT_CREATED_NOOP = 1 << (32 + 1);
    uint constant public BEFORE_AGREEMENT_UPDATED_NOOP = 1 << (32 + 2);
    uint constant public AFTER_AGREEMENT_UPDATED_NOOP = 1 << (32 + 3);
    uint constant public BEFORE_AGREEMENT_TERMINATED_NOOP = 1 << (32 + 4);

}
