pragma solidity 0.5.17;

import "./ISuperAgreement.sol";

/**
 * @title Superfluid's base super agreement
 * @notice 
 * @author Superfluid
 */
contract SuperAgreementBase is ISuperAgreement {

    function getState(
        ISuperToken token,
        address account) external view
        returns (bytes memory currentState) {
        currentState = token.getState(address(this), account);
    }

    function updateState(
        ISuperToken token,
        address account,
        bytes calldata additionalState) external {
        bytes memory currentState = token.getState(address(this), account);
        bytes memory newState = this.composeState(currentState, additionalState);
        token.updateState(address(this), account, newState);
    }

}
