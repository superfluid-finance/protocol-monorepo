// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

/**
 * @title Stream Butler interface
 * @author Superfluid
 */
interface IStreamButler {
    struct ButlerFlowData {
        address token;
        address receiver;
        uint256 endTime; // time in future
        uint8 index;
    }

    /**
     * @notice Creates a butler flow data record so the resolver knows when to close the flow.
     * @dev Cannot create a new record if one with the same id exists.
     * @param _token the token being streamed
     * @param _receiver the flow receiver
     * @param _endTime the end time of the flow
     */
    function createButlerFlowData(
        address _token,
        address _receiver,
        uint32 _endTime
    ) external;

    /**
     * @notice Gets the butler flow data struct saved in mapping by the _flowId
     * @dev This will return an object with empty (default) values if it doesn't exist
     * @param _flowId the flow id we are interested in
     */
    function getButlerFlowData(bytes32 _flowId)
        external
        view
        returns (ButlerFlowData memory);

    /**
     * @notice Gets the list of flow ids that currently exist.
     */
    function getFlowIds() external view returns (bytes32[] memory);

    /**
     * @notice Gets the list of flow ids that will be tracked by the resolver.
     */
    function canCloseFlow(bytes32 _agreementId) external view returns (bool);

    /**
     * @notice Creates a flow via the stream butler and creates ButlerFlowData for the resolver.
     * @dev the ButlerFlowData is necessary for the resolver to close the stream.
     * @param _token the token being streamed
     * @param _receiver the flow receiver
     * @param _flowRate the desired flowrate
     * @param _endTime the end time of the flow
     */
    function serveFlow(
        address _token,
        address _receiver,
        int96 _flowRate,
        uint32 _endTime
    ) external;

    /**
     * @notice Updates a flow via the stream butler and updates ButlerFlowData for the resolver.
     * @dev the ButlerFlowData is necessary for the resolver to close the stream.
     * @param _token the token being streamed
     * @param _receiver the flow receiver
     * @param _flowRate the desired flowrate
     * @param _endTime the end time of the flow
     */
    function adjustFlow(
        address _token,
        address _receiver,
        int96 _flowRate,
        uint256 _endTime
    ) external;

    /**
     * @notice Closes a flow via the stream butler and updates ButlerFlowData for the resolver.
     * @param _token the token being streamed
     * @param _receiver the flow receiver
     */
    function closeFlow(address _token, address _receiver) external;
}
