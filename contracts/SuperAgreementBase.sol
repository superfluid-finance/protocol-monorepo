pragma solidity 0.6.6;

import "./ISuperAgreement.sol";

/**
 * @title Superfluid's base super agreement implementation
 * @author Superfluid
 */
abstract // stupid solhint
contract SuperAgreementBase is ISuperAgreement {

    function composeState(
        bytes memory currentState,
        bytes memory additionalState)
        internal pure virtual
        returns (bytes memory newState);

    function getState(
        ISuperToken token,
        address account)
        internal view
        returns (bytes memory currentState)
    {
        currentState = token.getState(address(this), account);
    }

    function updateState(
        ISuperToken token,
        address account,
        bytes memory additionalState) internal {
        bytes memory currentState = token.getState(address(this), account);
        bytes memory newState = composeState(currentState, additionalState);
        token.updateState(account, newState);
    }

}
