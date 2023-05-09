// SPDX-License-Identifier: AGPLv3
pragma solidity >=0.8.4;

import {ISuperAgreement} from "../superfluid/ISuperAgreement.sol";
import {ISuperfluidToken} from "../superfluid/ISuperfluidToken.sol";
import {ISuperfluidPool, ISuperfluidPoolAdmin} from "../superfluid/ISuperfluidPool.sol";

/**
 * @title General Distribution Agreement interface
 * @author Superfluid
 */
abstract contract IGeneralDistributionAgreementV1 is ISuperAgreement, ISuperfluidPoolAdmin {
    // Custom Errors
    error GDA_DISTRIBUTE_FOR_OTHERS_NOT_ALLOWED();  // 0xf67d263e
    error GDA_NON_CRITICAL_SENDER();                // 0x666f381d
    error GDA_INSUFFICIENT_BALANCE();               // 0x33115c3f
    error GDA_NO_NEGATIVE_FLOW_RATE();              // 0x15f25663
    error GDA_ONLY_SUPER_TOKEN_POOL();              // 0x90028c37

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

    event PoolCreated(ISuperfluidToken indexed token, address indexed admin, ISuperfluidPool pool);

    event PoolConnectionUpdated(
        ISuperfluidToken indexed token, address indexed account, ISuperfluidPool indexed pool, bool connected
    );

    event UniversalIndexUpdated(
        ISuperfluidToken indexed token, address indexed account, uint32 settledAt, int256 settledValue, int96 flowRate
    );

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.GeneralDistributionAgreement.v1");
    }

    /// @dev Gets the GDA net flow rate of `account` for `token`.
    /// @param token The token address
    /// @param account The account address
    /// @return net flow rate
    function getNetFlowRate(ISuperfluidToken token, address account) external view virtual returns (int96);

    /// @notice Gets the GDA flow rate of `from` to `to` for `token`.
    /// @dev This is primarily used to get the flow distribution flow rate from a distributor to a pool or the
    /// adjustment flow rate of a pool.
    /// @param token The token address
    /// @param from The sender address
    /// @param to The receiver address
    /// @return flow rate
    function getFlowRate(ISuperfluidToken token, address from, address to) external view virtual returns (int96);

    /// @notice Gets the actual flow distribution flow rate of `from` to `to` for `token` given `requestedFlowRate`.
    /// the actual flow distribution flow rate is the flow rate that will be sent from `from`.
    /// @dev The difference between the requested flow rate and the actual flow rate is the adjustment flow rate.
    /// @param token The token address
    /// @param from The sender address
    /// @param to The pool address
    /// @param requestedFlowRate The requested flow rate
    /// @return actualFlowRate
    function getFlowDistributionActualFlowRate(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool to,
        int96 requestedFlowRate
    ) external view virtual returns (int96 actualFlowRate);

    /// @notice Gets the adjustment flow rate of `pool` for `token`.
    /// @param token The token address
    /// @param pool The pool address
    /// @return adjustment flow rate
    function getPoolAdjustmentFlowRate(address token, address pool) external view virtual returns (int96);

    ////////////////////////////////////////////////////////////////////////////////
    // Pool Operations
    ////////////////////////////////////////////////////////////////////////////////

    /// @notice Creates a new pool for `token` where the admin is `admin`.
    /// @param admin The admin of the pool
    /// @param token The token address
    function createPool(address admin, ISuperfluidToken token) external virtual returns (ISuperfluidPool pool);

    /// @notice Connects `msg.sender` to `pool`.
    /// @dev This is used to connect a pool to the GDA.
    /// @param pool The pool address
    /// @param ctx Context bytes (see ISuperfluid.sol for Context struct)
    /// @return newCtx the new context bytes
    function connectPool(ISuperfluidPool pool, bytes calldata ctx) external virtual returns (bytes memory newCtx);

    /// @notice Disconnects `msg.sender` from `pool`.
    /// @dev This is used to disconnect a pool from the GDA.
    /// @param pool The pool address
    /// @param ctx Context bytes (see ISuperfluidPoolAdmin for Context struct)
    /// @return newCtx the new context bytes
    function disconnectPool(ISuperfluidPool pool, bytes calldata ctx) external virtual returns (bytes memory newCtx);

    /// @notice Checks whether `account` is a pool.
    /// @param token The token address
    /// @param account The account address
    /// @return true if `account` is a pool
    function isPool(ISuperfluidToken token, address account) external view virtual returns (bool);

    ////////////////////////////////////////////////////////////////////////////////
    // Agreement Operations
    ////////////////////////////////////////////////////////////////////////////////

    /// @notice Tries to distribute `requestedAmount` of `token` from `from` to `pool`.
    /// @dev NOTE: The actual amount distributed may differ.
    /// @param token The token address
    /// @param from The sender address
    /// @param pool The pool address
    /// @param requestedAmount The requested amount
    /// @param ctx Context bytes (see ISuperfluidPool for Context struct)
    /// @return newCtx the new context bytes
    function distribute(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        uint256 requestedAmount,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);

    /// @notice Tries to distributeFlow `requestedFlowRate` of `token` from `from` to `pool`.
    /// @dev NOTE: The actual distribution flow rate may differ.
    /// @param token The token address
    /// @param from The sender address
    /// @param pool The pool address
    /// @param requestedFlowRate The requested flow rate
    /// @param ctx Context bytes (see ISuperfluidPool for Context struct)
    /// @return newCtx the new context bytes
    function distributeFlow(
        ISuperfluidToken token,
        address from,
        ISuperfluidPool pool,
        int96 requestedFlowRate,
        bytes calldata ctx
    ) external virtual returns (bytes memory newCtx);
}
