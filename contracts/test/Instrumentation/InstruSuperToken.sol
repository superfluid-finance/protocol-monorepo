pragma solidity 0.6.6;

import "./IInstruSuperToken.sol";
import "./InstruEventsDebug.sol";

contract InstruSuperToken is InstruEventsDebug {

    IInstrSuperToken public target;

    constructor(address _wrapping) public {
        target = IInstrSuperToken(_wrapping);
    }

   function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        returns(uint256 blocktime, bytes memory state)
    {
        emit LogAddress("agreementClass", agreementClass);
        emit LogBytes32("id", id);
        bytes memory _state = target.getAgreementData(agreementClass, id);
        return (block.timestamp, _state);
    }

    function balanceOf(
        address account
    )
        public
        view
        returns (uint256 blocktime, uint256 balance)
    {
        uint256 _balance = target.balanceOf(account);
        return (block.timestamp, _balance);
    }

    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
        public
        view
        returns (uint256 blocktime, int256 balance)
    {
        int256 _balance = target.realtimeBalanceOf(account, timestamp);
        return (block.timestamp, _balance);
    }

    function getAgreementAccountState(
        address account
    )
        external
        view
        returns (uint256 blocktime, bytes memory data)
    {
        bytes memory _state = target.getAgreementAccountState(account);
        return (block.timestamp, _state);
    }

    function nowBalanceOf(
        address account
    )
        external
        view
        returns (uint256 blocktime, int256 balance)
    {
        int256 _balance = target.realtimeBalanceOf(account, block.timestamp);
        return (block.timestamp, _balance);
    }
}
