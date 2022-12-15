// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity ^0.8.0;

import { ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

interface IStrategy {

    event Wrapped(
        address indexed user,
        address indexed superToken,
        uint256 superTokenAmount
    );
    event ManagerChanged(
        address indexed oldManager,
        address indexed manager
    );
    event EmergencyWithdrawInitiated(
        address indexed receiver,
        address indexed token,
        uint256 amount
    );

    /// Custom error to indicate that null address has been passed.
    error ZeroAddress();

    /// Custom error to indicate that supertoken provided isn't supported.
    /// @param superToken Address of the supertoken which isn't supported.
    error UnsupportedSuperToken(address superToken);

    /// Custom error to indicate that the caller is unauthorized to call a function.
    /// @param caller Address of the caller of the function.
    /// @param expectedCaller Address of the expected caller of the function.
    error UnauthorizedCaller(address caller, address expectedCaller);

    /// Function to get the current Manager contract which interacts with this contract.
    /// strategy contract.
    /// @return Manager contract address.
    function manager() external returns (address);

    /// Function to wrap schedule an account based on certain conditions pre-defined in the Manager contract.
    /// @param user Address of the user whose account needs to be topped-up.
    /// @param superToken Supertoken which needs to be replenished.
    /// @param superTokenAmount Amount of supertoken to be replenished.
    /// @dev This function assumes whatever given by Manager is correct. Therefore, all the necessary-
    /// checks such as if a wrap schedule is required and if so how much amount needs to be topped up, do we have-
    /// enough allowance to perform a wrap schedule and so on must be performed in Manager only.
    function wrap(
        address user,
        ISuperToken superToken,
        uint256 superTokenAmount
    ) external;

    /// Function to check whether a supertoken is supported by a strategy or not.
    /// @dev More specifically, this function checks whether the underlying token of the supertoken-
    /// is supported or not.
    /// @param superToken Supertoken which needs to be checked for support.
    /// @return Boolean indicating the support of the supertoken.
    function isSupportedSuperToken(ISuperToken superToken)
        external
        view
        returns (bool);

    /// Function to change the Manager contract that a strategy interacts with.
    /// This function can only be called by the owner of the strategy contract.
    /// @param newManager Address of the new Manager contract the strategy should interact with.
    function changeManager(address newManager) external;

    /// Function to withdraw any token locked in the contract in case of an emergency.
    /// Ideally, no tokens should ever be sent directly to the contract but in case it happens,
    /// this function can be used by the owner of the strategy contract to transfer all the locked tokens-
    /// to their address.
    /// @param token Address of the locked token which is to be transferred to the owner address.
    function emergencyWithdraw(address token) external;
}
