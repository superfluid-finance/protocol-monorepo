pragma solidity 0.6.6;

import "./IInstruSuperToken.sol";
import "./InstruEventsDebug.sol";

contract InstruSuperToken is InstruEventsDebug {

    IInstrSuperToken public target;

    constructor(address _wrapping) public {
        target = IInstrSuperToken(_wrapping);
    }

    function getState
    (
        address agreementClass,
        address account
    )
    external
    returns (uint256 blocktime, bytes memory state)
    {
        emit LogAddress("agreementClass", agreementClass);
        emit LogAddress("account", account);
        bytes memory _state = target.getState(agreementClass, account);
        return (block.timestamp, _state);
    }

    function balanceOf(
        address account
    )
        public
        returns (uint256 blocktime, int256 balance)
    {
        int256 _balance = target.balanceOf(account);
        return (block.timestamp, _balance);
    }

    function currentState(
        address sender,
        address receiver
    )
        external
        returns(uint256 blocktime, bytes memory state)
    {

        bytes memory _state = target.currentState(sender, receiver);
        return (block.timestamp, _state);
    }
}
