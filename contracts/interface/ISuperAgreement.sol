pragma solidity >= 0.6.0;

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    /// @notice Calculate the real balance from the state
    /// @param state State to be query
    /// @param time Time of balance
    /// @return amount Account real-time balance
    function realtimeBalanceOf(
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns (int256 amount);

    /// @notice Calculate the new state if settled balance is reset
    /// @param state State to be query
    /// @param time Time of balance
    /// @return newState New agreement account state
    function touch(
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns(bytes memory newState);

}
