pragma solidity 0.6.6;

import "./interface/ISuperAgreement.sol";
import "./interface/ISuperToken.sol";

/**
 * @title Superfluid's base super agreement implementation
 * @author Superfluid
 */

abstract
contract SuperAgreementBase is ISuperAgreement {

    function composeState(
        bytes memory currentState,
        bytes memory additionalState
    )
        internal
        pure
        virtual
        returns (bytes memory newState);

    function mirrorState(
        bytes memory state
    )
        internal
        pure
        virtual
        returns(bytes memory mirror);

    function getState(
        ISuperToken token,
        address account
    )
        internal
        view
        returns (bytes memory currentState)
    {
        currentState = token.getState(address(this), account);
    }

    function updateState(
        ISuperToken token,
        address sender,
        address receiver,
        bytes memory additionalState
    )
        public
    {
        //sender
        bytes memory _currentSenderState = token.getState(address(this), sender);
        bytes memory _newSenderState = composeState(_currentSenderState, mirrorState(additionalState));
        //receiver
        bytes memory _currentReceiverState = token.getState(address(this), receiver);
        bytes memory _newReceiverState = composeState(_currentReceiverState, additionalState);

        token.updateState(sender, receiver, _newSenderState, _newReceiverState);
    }
}
