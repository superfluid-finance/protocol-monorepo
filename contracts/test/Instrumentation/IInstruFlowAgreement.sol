pragma solidity 0.6.6;

import "../../interface/ISuperToken.sol";

interface IInstruFlowAgreement {

    function balanceOf
    (
        bytes calldata state,
        uint256 time
    )
        external
        pure
        returns (int256 amount);

    function createFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external;

    function updateFlow
    (
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external;

    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external;

    function getFlowRate(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        returns(int256 flowRate);

    function getTotalInFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns(int256);

    function getTotalOutFlowRate(
        ISuperToken token,
        address account
    )
        external
        view
        returns(int256);

    function updateAccount(
        bytes calldata newState
    )
        external
        pure
        returns(int256 flowRate);

    function touch(
        bytes calldata currentState,
        uint256 timestamp
    )
    external
    pure
    returns(bytes memory newState);

}
