// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { Proxiable } from "../upgradability/Proxiable.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";


abstract contract AgreementBase is
    Proxiable,
    ISuperAgreement
{
    address private _host;

    function initialize() external {
        Proxiable._initialize();
        _host = msg.sender;
    }

    function proxiableUUID() public view override returns (bytes32) {
        return ISuperAgreement(this).agreementType();
    }

    function updateCode(address newAddress) external override {
        require(msg.sender == _host, "SF: only host can update code");
        return _updateCodeAddress(newAddress);
    }

}
