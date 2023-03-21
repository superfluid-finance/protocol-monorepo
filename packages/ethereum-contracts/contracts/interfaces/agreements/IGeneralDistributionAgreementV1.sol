// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperAgreement } from "../superfluid/ISuperAgreement.sol";
import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";
import {
    SuperTokenPool,
    ISuperTokenPool
} from "../../superfluid/SuperTokenPool.sol";

/**
 * @title General Distribution Agreement interface
 * @author Superfluid
 */
abstract contract IGeneralDistributionAgreementV1 is ISuperAgreement {
    error NO_NEGATIVE_UNITS();
    error NOT_POOL_ADMIN();
    error ONLY_SUPER_TOKEN_POOL();

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.agreements.GeneralDistributionAgreement.v1"
            );
    }

    function getNetFlowRate(
        address account
    ) external view virtual returns (int96);

    function getFlowRate(
        address from,
        address to
    ) external view virtual returns (int96);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////
    function createPool(
        address admin,
        ISuperfluidToken token
    ) external virtual returns (SuperTokenPool pool);

    // function connectPool(
    //     ISuperTokenPool pool,
    //     bytes calldata ctx
    // ) external virtual returns (bytes memory newCtx);

    function disconnectPool(
        ISuperTokenPool pool,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    // function connectPool(
    //     ISuperTokenPool pool,
    //     bool doConnect,
    //     bytes calldata ctx
    // ) public virtual returns (bytes memory newCtx);

    function isMemberConnected(
        address pool,
        address member
    ) external view virtual returns (bool);

    ////////////////////////////////////////////////////////////////////////////////
    // Agreement Operations
    ////////////////////////////////////////////////////////////////////////////////

    function distribute(
        ISuperfluidToken token,
        SuperTokenPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    function distributeFlow(
        ISuperfluidToken token,
        SuperTokenPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);
}
