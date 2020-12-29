// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";


abstract contract AgreementBase is
    UUPSProxiable,
    ISuperAgreement
{
    address private _host;

    function initialize()
        external override
        initializer // OpenZeppelin Initializable
    {
        _host = msg.sender;
    }

    function proxiableUUID()
        public view override
        returns (bytes32)
    {
        return ISuperAgreement(this).agreementType();
    }

    function updateCode(address newAddress)
        external override
    {
        require(msg.sender == _host, "only host can update code");
        return _updateCodeAddress(newAddress);
    }

}
