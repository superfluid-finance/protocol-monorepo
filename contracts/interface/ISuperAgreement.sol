// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    /// @dev Returns the type of the agreement class
    function agreementType() external pure returns (bytes32);

    /// @notice Calculate the real-time balance using the state.
    /// @param state State to be used.
    /// @param time Future time used for the calculation.
    /// @return amount Account real-time balance.
    function realtimeBalanceOf(
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns (int256 amount); // TODO add `deposit` to the return list

    /// @notice Change the timestamp of the state.
    /// @param state State to be used.
    /// @param time Time for the new state.
    /// @return newState New state.
    function touch(
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns(bytes memory newState);

}
