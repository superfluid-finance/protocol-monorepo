pragma solidity >= 0.5.0;

import { ISuperToken } from "./ISuperToken.sol";

/**
 * @title Superfluid's agreement interface
 * @notice 
 * @author Superfluid
 */
interface ISuperAgreement {

    function balanceOf(bytes calldata state) external pure
        returns (uint256 amount);

    function composeState(
        bytes calldata currentState,
        bytes calldata additionalState) external pure
        returns (bytes memory newState);

    function getState(
        ISuperToken token,
        address account) external view
        returns (bytes memory currentState);

    function updateState(
        ISuperToken token,
        address account,
        bytes calldata additionalState) external;

}
