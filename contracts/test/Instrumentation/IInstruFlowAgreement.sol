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
}
