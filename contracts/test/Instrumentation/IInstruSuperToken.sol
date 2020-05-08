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
