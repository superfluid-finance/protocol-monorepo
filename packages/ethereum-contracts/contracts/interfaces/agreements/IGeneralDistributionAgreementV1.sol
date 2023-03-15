// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperAgreement } from "../superfluid/ISuperAgreement.sol";
import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";
import { ISuperTokenPool } from "../superfluid/ISuperTokenPool.sol";

/**
 * @title General Distribution Agreement interface
 * @author Superfluid
 */
abstract contract IGeneralDistributionAgreementV1 is ISuperAgreement {
    error NO_NEGATIVE_UNITS();
    error NOT_POOL_ADMIN();

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.GeneralDistributionAgreement.v1");
    }

    // function distribute(
    //     ISuperfluidToken token,
    //     ISuperTokenPool to,
    //     uint256 amount,
    //     bytes calldata ctx
    // ) external virtual returns(bytes memory newCtx);

    // function distributeFlow(
    //     ISuperfluidToken token,
    //     ISuperTokenPool to,
    //     int96 rqeuestedFlowRate,
    //     bytes calldata ctx
    // ) external virtual returns(bytes memory newCtx);
}
