pragma solidity 0.6.6;

import "./IInstruFlowAgreement.sol";
import "./InstruEventsDebug.sol";

contract InstruFlowAgreement {

    IInstruFlowAgreement public target;

    constructor(address _wrapping) public {
        target = IInstruFlowAgreement(_wrapping);
    }

    function balanceOf
    (
        bytes calldata state,
        uint256 time
    )
        external
        view
        returns (uint256 blocktime, int256 amount)
    {
        int256 _amount = target.balanceOf(state, time);
        return (block.timestamp, _amount);
    }

    function getFlowRate(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        returns(uint256 blocktime, int256 flowRate)
    {
        int256 _flowRate = target.getFlowRate(token, sender, receiver);
        return (block.timestamp, _flowRate);
    }

    function getTotalInFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns(uint256 blocktime, int256 flowRate)
    {
        int256 _flowRate = target.getTotalInFlowRate(token, account);
        return (block.timestamp, _flowRate);
    }

    function getTotalOutFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns(uint256 blocktime, int256 flowRate)
    {
        int256 _flowRate = target.getTotalOutFlowRate(token, account);
        return (block.timestamp, _flowRate);
    }
}
