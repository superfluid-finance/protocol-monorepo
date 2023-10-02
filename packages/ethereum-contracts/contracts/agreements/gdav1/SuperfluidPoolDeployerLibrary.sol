// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { BeaconProxy } from "@openzeppelin/contracts/proxy/beacon/BeaconProxy.sol";
import { ISuperfluidToken } from "../../interfaces/superfluid/ISuperfluidToken.sol";
import { SuperfluidPool } from "./SuperfluidPool.sol";

library SuperfluidPoolDeployerLibrary {
    function deploy(
        address beacon,
        address admin,
        ISuperfluidToken token,
        bool transferabilityForUnitsOwner,
        bool distributionFromAnyAddress
    ) external returns (SuperfluidPool pool) {
        bytes memory initializeCallData = abi.encodeWithSelector(
            SuperfluidPool.initialize.selector,
            admin,
            token,
            transferabilityForUnitsOwner,
            distributionFromAnyAddress
        );
        BeaconProxy superfluidPoolBeaconProxy = new BeaconProxy(
            beacon,
            initializeCallData
        );
        pool = SuperfluidPool(address(superfluidPoolBeaconProxy));
    }
}
