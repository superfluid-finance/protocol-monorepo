pragma solidity 0.6.6;


interface IInstrSuperToken {

   function getAgreementData(
        address agreementClass,
        bytes32 id
    )
        external
        view
        returns(bytes memory state);

    function balanceOf(
        address account
    )
        external
        view
        returns (uint256 balance);

    function realtimeBalanceOf(
        address account,
        uint256 timestamp
    )
        external
        view
        returns (int256 balance);

    function getAgreementAccountState(
        address account
    )
        external
        view
        returns (bytes memory data);

}
