// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import { ISuperToken } from "./ISuperToken.sol";

/**
 * @title Superfluid's agreement interface
 * @author Superfluid
 */
interface ISuperAgreement {

    /// @dev Returns the type of the agreement class
    function agreementType() external pure returns (bytes32);

    /// @notice Calculate the real-time balance using the state.
    /// @param account Account the state belongs to
    /// @param state State to be used.
    /// @param time Future time used for the calculation.
    /// @return dynamicBalance Dynamic balance portion of real-time balance of this agreement.
    /// @return deposit Account deposit amount of this agreement.
    /// @return owedDeposit Account owed deposit amount of this agreement.
    function realtimeBalanceOf(
        ISuperToken token,
        address account,
        bytes calldata state,
        uint256 time
    )
        external
        view
        returns (
            int256 dynamicBalance,
            int256 deposit,
            int256 owedDeposit
        );

    /// @notice Change the timestamp of the state.
    /// @param account Account the state belongs to
    /// @param state State to be used.
    /// @param time Time for the new state.
    /// @return newState New state.
    function touch(
        address account,
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns(bytes memory newState);

}
