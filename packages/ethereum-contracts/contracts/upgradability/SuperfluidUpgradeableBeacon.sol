// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    UpgradeableBeacon
} from "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";

contract SuperfluidUpgradeableBeacon is UpgradeableBeacon {
    error ZERO_ADDRESS_IMPLEMENTATION();    // 0x80883162
    error INCOMPATIBLE_LOGIC();             // 0x5af2144c
    error NO_PROXY_LOOP();                  // 0z66750bca

    constructor(address implementation_) UpgradeableBeacon(implementation_) {}

    function upgradeTo(address newImplementation) public override onlyOwner {
        if (newImplementation == address(0)) {
            revert ZERO_ADDRESS_IMPLEMENTATION();
        }
        if (
            SuperTokenPool(newImplementation).proxiableUUID() !=
            SuperTokenPool(implementation()).proxiableUUID()
        ) {
            revert INCOMPATIBLE_LOGIC();
        }
        if (newImplementation == address(this)) {
            revert NO_PROXY_LOOP();
        }
        super.upgradeTo(newImplementation);
    }
}
