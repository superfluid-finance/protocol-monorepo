// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    ISuperToken
} from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

interface IFlowScheduler {
    error TimeWindowInvalid();
    error AccountInvalid();
    error ZeroAddress();
    error HostInvalid();
    error ScheduleInvalid();
    error UserDataInvalid();

    struct FlowSchedule {
        uint32 startDate;
        uint32 startMaxDelay;
        uint32 endDate;
        int96 flowRate;
        uint256 startAmount;
        bytes32 userData;
    }

    /**
     * @dev Event emitted on creation of a new flow schedule
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     * @param startDate The timestamp when the stream should start (or 0 if starting not required)
     * @param startMaxDelay How many seconds the stream is allowed to start after startDate
     * @param flowRate The flowRate for the stream (or 0 if starting not required)
     * @param endDate The timestamp when the stream should stop (or 0 if closing not required)
     * @param startAmount The amount to be transferred at startDate additionally to starting a stream
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     */
    event FlowScheduleCreated(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 startDate,
        uint32 startMaxDelay,
        int96 flowRate,
        uint32 endDate,
        uint256 startAmount,
        bytes userData
    );

    /**
     * @dev Event emitted on deletion of a flow schedule
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     */
    event FlowScheduleDeleted(ISuperToken indexed superToken, address indexed sender, address indexed receiver);

    /**
     * @dev Emitted when the start of a stream is executed
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     * @param startDate The timestamp when the stream should start (or 0 if starting not required)
     * @param startMaxDelay How many seconds the stream is allowed to start after startDate
     * @param flowRate The flowRate for the stream (or 0 if starting not required)
     * @param startAmount The amount you would like to transfer at the startDate when you start streaming
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     */
    event CreateFlowExecuted(
        ISuperToken indexed superToken,
        address indexed sender,
        address indexed receiver,
        uint32 startDate,
        uint32 startMaxDelay,
        int96 flowRate,
        uint256 startAmount,
        bytes userData
    );

    /**
     * @dev Emitted when the stopping of a stream is executed
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     * @param endDate The timestamp when the stream should stop (or 0 if closing not required)
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     */
    event DeleteFlowExecuted(
        ISuperToken indexed superToken, address indexed sender, address indexed receiver, uint32 endDate, bytes userData
    );

    /**
     * @dev Creates a new flow schedule
     * @param superToken The SuperToken to be sent
     * @param receiver The receiver account
     * @param startDate The timestamp when the stream should start (or 0 if starting not required)
     * @param startMaxDelay How many seconds the stream is allowed to start after startDate
     * @param flowRate The flowRate for the stream (or 0 if starting not required)
     * @param startAmount The amount to be transferred at startDate additionally to starting a stream
     * @param endDate The timestamp when the stream should stop (or 0 if closing not required)
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     * @param ctx Superfluid context used when batching operations. (or bytes(0) if not SF batching)
     */
    function createFlowSchedule(
        ISuperToken superToken,
        address receiver,
        uint32 startDate,
        uint32 startMaxDelay,
        int96 flowRate,
        uint256 startAmount,
        uint32 endDate,
        bytes memory userData,
        bytes memory ctx
    ) external returns (bytes memory newCtx);

    /**
     * @dev Deletes a flow schedule
     * @param superToken The SuperToken to be sent
     * @param receiver The receiver account
     * @param ctx Superfluid context used when batching operations. (or bytes(0) if not SF batching)
     */
    function deleteFlowSchedule(ISuperToken superToken, address receiver, bytes memory ctx)
        external
        returns (bytes memory newCtx);

    /**
     * @dev Executes the starting of a stream
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     */
    function executeCreateFlow(ISuperToken superToken, address sender, address receiver, bytes memory userData)
        external
        returns (bool success);

    /**
     * @dev Executes the stopping of a stream
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     * @param userData Arbitrary UserData to be added to the stream (or bytes(0) if no data needed)
     */
    function executeDeleteFlow(ISuperToken superToken, address sender, address receiver, bytes memory userData)
        external
        returns (bool success);

    /**
     * @dev Get data currently stored for a flow schedule
     * @param superToken The SuperToken to be sent
     * @param sender The sender account
     * @param receiver The receiver account
     */
    function getFlowSchedule(address superToken, address sender, address receiver)
        external
        view
        returns (FlowSchedule memory);
}
