pragma solidity 0.6.6;

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    /// @notice Calculate the real balance of a user, taking in consideration all flows of tokens
    /// @param state State to be query
    /// @param time Time of balance
    /// @return amount Account balance
    function balanceOf(bytes calldata state, uint256 time)
        external
        pure
        returns (int256 amount);

    function encodeFlow(uint256 timestamp, int256 flowRate) external pure returns(bytes memory state);

    function decodeFlow(bytes calldata state) external pure returns(uint256, int256);

    function touch(
        bytes calldata currentState,
        uint256 timestamp
    )
        external
        pure
        returns(bytes memory newState);
        
}
