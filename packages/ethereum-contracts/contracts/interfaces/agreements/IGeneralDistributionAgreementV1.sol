// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import { ISuperAgreement } from "../superfluid/ISuperAgreement.sol";
import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";
import {
    ISuperfluidPool,
    ISuperfluidPoolAdmin
} from "../superfluid/ISuperfluidPool.sol";

/**
 * @title General Distribution Agreement interface
 * @author Superfluid
 */
abstract contract IGeneralDistributionAgreementV1 is
    ISuperAgreement,
    ISuperfluidPoolAdmin
{
    // Custom Errors
    error GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();
    error GDA_NON_CRITICAL_SENDER();
    error GDA_INSUFFICIENT_BALANCE();
    error GDA_NO_NEGATIVE_DISTRIBUTION();
    error GDA_NO_NEGATIVE_FLOW_RATE();
    error GDA_ONLY_SUPER_TOKEN_POOL();

    // Events
    event InstantDistributionUpdated(
        ISuperfluidToken indexed token,
        ISuperfluidPool indexed pool,
        address indexed distributor,
        uint256 distributedAtTimestamp,
        uint256 requestedAmount,
        uint256 actualAmount
    );

    event FlowDistributionUpdated(
        ISuperfluidToken indexed token,
        ISuperfluidPool indexed pool,
        address operator,
        address indexed distributor,
        uint256 distributedAtTimestamp,
        int96 oldFlowRate,
        int96 newDistributorToPoolFlowRate,
        int96 newTotalDistributionFlowRate
    );

    event PoolCreated(
        ISuperfluidToken indexed token,
        address indexed admin,
        ISuperfluidPool pool
    );

    event PoolConnectionUpdated(
        ISuperfluidToken indexed token,
        address indexed account,
        ISuperfluidPool indexed pool,
        bool connected
    );

    event UniversalIndexUpdated(
        ISuperfluidToken indexed token,
        address indexed account,
        uint32 settledAt,
        int256 settledValue,
        int96 flowRate
    );

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.agreements.GeneralDistributionAgreement.v1"
            );
    }

    function getNetFlowRate(
        ISuperfluidToken token,
        address account
    ) external view virtual returns (int96);

    function getFlowRate(
        ISuperfluidToken token,
        address from,
        address to
    ) external view virtual returns (int96);

    function getFlowDistributionActualFlowRate(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool to,
        int96 requestedFlowRate
    ) external view virtual returns (int96 finalFlowRate);

    function realtimeBalanceVectorAt(
        ISuperfluidToken token,
        address account,
        uint256 time
    )
        public
        view
        virtual
        returns (int256 own, int256 fromPools, int256 deposit);

    function realtimeBalanceOfNow(
        ISuperfluidToken token,
        address account
    ) external view virtual returns (int256 rtb);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////
    function createPool(
        address admin,
        ISuperfluidToken token
    ) external virtual returns (ISuperfluidPool pool);

    function connectPool(
        ISuperfluidPool pool,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    function disconnectPool(
        ISuperfluidPool pool,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    function isPool(
        ISuperfluidToken token,
        address account
    ) external view virtual returns (bool);

    ////////////////////////////////////////////////////////////////////////////////
    // Agreement Operations
    ////////////////////////////////////////////////////////////////////////////////

    function distribute(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    function distributeFlow(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);
}
