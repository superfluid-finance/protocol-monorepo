// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";
import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";
import { SuperfluidGovernanceBase } from "./SuperfluidGovernanceBase.sol";
import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

/**
 * @title A proxy for governance which is both ownable and upgradable
 * @author Superfluid
 * IMPORTANT! Make sure the inheritance order remains in sync with the logic contract (Ownable first)!
 */
// solhint-disable-next-line no-empty-blocks
contract SuperfluidGovernanceIIProxy is Ownable, UUPSProxy { }

contract SuperfluidGovernanceII is
    Ownable,
    UUPSProxiable,
    SuperfluidGovernanceBase
{
    function _requireAuthorised() private view {
        require(owner() == _msgSender(), "SFGovII: only owner is authorized");
    }

    /**************************************************************************
    * UUPSProxiable
    **************************************************************************/

    // TODO: do we need initialize() here?

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperfluidGovernanceII.implementation");
    }

    function updateCode(address newAddress)
        external override
    {
        _requireAuthorised();
        _updateCodeAddress(newAddress);
    }

    /**************************************************************************
    * SuperfluidGovernanceBase
    **************************************************************************/

    function _requireAuthorised(ISuperfluid /*host*/)
        internal view override
    {
        _requireAuthorised();
    }
}
