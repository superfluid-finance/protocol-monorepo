// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

import { Proxiable } from "../upgradability/Proxiable.sol";
import { Ownable } from "../access/Ownable.sol";
import { ISuperAgreement } from "../interfaces/superfluid/ISuperAgreement.sol";


abstract contract AgreementBase is
    Proxiable,
    Ownable,
    ISuperAgreement
{

    function initialize() external {
        Proxiable._initialize();
        _owner = msg.sender;
    }

    function proxiableUUID() public view override returns (bytes32) {
        return ISuperAgreement(this).agreementType();
    }

    function updateCode(address newAddress) external onlyOwner {
        return _updateCodeAddress(newAddress);
    }

}
