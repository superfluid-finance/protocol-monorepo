pragma solidity >= 0.6.0;

import "./ISuperToken.sol";
import "./ISuperAgreement.sol";

abstract contract IFlowAgreement is ISuperAgreement {

    function createFlow(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        external
        virtual;

    function getFlow(
       ISuperToken token,
       address sender,
       address receiver
    )
        external
        view
        virtual
        returns (int256 flowRate);

    function updateFlow(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        external
        virtual;

    function deleteFlow(
        ISuperToken token,
        address account
    )
        external
        virtual;

}
