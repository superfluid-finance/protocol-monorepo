// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import {
    BeaconProxy
} from "@openzeppelin/contracts/proxy/beacon/BeaconProxy.sol";
import {
    GeneralDistributionAgreementV1
} from "../agreements/GeneralDistributionAgreementV1.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";

library SuperTokenPoolDeployerLibrary {
    function deploy(
        address beacon,
        address admin,
        GeneralDistributionAgreementV1 gdaV1,
        ISuperfluidToken token
    ) external returns (SuperTokenPool pool) {
        bytes memory initializeCallData = abi.encodeWithSelector(
            SuperTokenPool.initialize.selector,
            admin,
            gdaV1,
            token
        );
        BeaconProxy superTokenPoolBeaconProxy = new BeaconProxy(
            beacon,
            initializeCallData
        );
        pool = SuperTokenPool(address(superTokenPoolBeaconProxy));
    }
}
