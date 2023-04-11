// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.19;

import { GeneralDistributionAgreementV1 } from "../agreements/GeneralDistributionAgreementV1.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { SuperTokenPool } from "../superfluid/SuperTokenPool.sol";

library SuperTokenPoolDeployerLibrary {
    function deploy(
        address admin,
        GeneralDistributionAgreementV1 gdaV1,
        ISuperfluidToken token
    ) external returns (SuperTokenPool pool) {
        pool = new SuperTokenPool(admin, gdaV1, token);
    }
}