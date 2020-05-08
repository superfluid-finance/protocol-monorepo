pragma solidity 0.6.6;


interface IInstrSuperToken {

    function getState
    (
        address agreementClass,
        address account
    )
    external
    view
    returns (bytes memory state);

    function updateState
    (
        address sender,
        address receiver,
        bool termination,
        bytes calldata senderState,
        bytes calldata receiverState
    )
        external;

    function upgrade(uint256 amount) external;

    function downgrade(uint256 amount) external;

    function balanceOf(
        address account
    )
    external
    view
    returns (int256 balance);

    function currentState
    (
        address sender,
        address receiver
    )
        external
        view
        returns(bytes memory state);

}
