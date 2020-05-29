pragma solidity >= 0.6.0;

import "./ISuperToken.sol";
import "./ISuperAgreement.sol";

/**
 * @title Superfluid's flow agreement interface
 * @author Superfluid
 */
abstract contract IFlowAgreement is ISuperAgreement {

    /// @notice Create a new flow between msg.sender and receiver
    /// @param token Super token address.
    /// @param receiver Flow recipient address.
    /// @param flowRate Flow rate in amount per second.
    function createFlow(
        ISuperToken token,
        address receiver,
        int256 flowRate
    )
        external
        virtual;

    /// @notice Flow created event
    /// @param token Super token address.
    /// @param receiver Flow recipient address.
    /// @param flowRate Flow rate in amount per second.
    event FlowCreated(
        ISuperToken indexed token,
        address indexed sender,
        address indexed receiver,
        int256 flowRate
    );

    /// @notice Get the new flow rate between sender and receiver
    /// @param token Super token address.
    /// @param receiver Flow sender address.
    /// @param receiver Flow recipient address.
    function getFlow(
       ISuperToken token,
       address sender,
       address receiver
    )
        external
        view
        virtual
        returns (int256 flowRate);

    /// @notice Update the flow between msg.sender and receiver
    /// @param token Super token address.
    /// @param receiver Flow recipient address.
    /// @param newFlowRate New flow rate in amount per second.
    function updateFlow(
        ISuperToken token,
        address receiver,
        int256 newFlowRate
    )
        external
        virtual;

    /// @notice Flow created event
    /// @param token Super token address.
    /// @param receiver Flow recipient address.
    /// @param newFlowRate Flow rate in amount per second.
    event FlowUpdated(
        ISuperToken indexed token,
        address indexed sender,
        address indexed receiver,
        int256 newFlowRate
    );

    /// @notice Delete the flow between msg.sender and receiver
    /// @param token Super token address.
    /// @param receiver Flow recipient address.
    function deleteFlow(
        ISuperToken token,
        address receiver
    )
        external
        virtual;

}
