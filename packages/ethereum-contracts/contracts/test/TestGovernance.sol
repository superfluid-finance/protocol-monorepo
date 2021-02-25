// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    ISuperfluidGovernance,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperfluidGovernanceBase } from "../gov/SuperfluidGovernanceBase.sol";

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";


contract TestGovernance is
    Ownable,
    SuperfluidGovernanceBase
{
    ISuperfluid private _host;

    function initialize(
        ISuperfluid host,
        address rewardAddress,
        uint256 liquidationPeriod,
        address[] memory trustedForwarders
    )
        external
    {
        _host = host;

        setRewardAddress(_host, ISuperfluidToken(address(0)), rewardAddress);
        setCFAv1LiquidationPeriod(_host, ISuperfluidToken(address(0)), liquidationPeriod);
        for (uint i = 0; i < trustedForwarders.length; ++i) {
            enableTrustedForwarder(_host, ISuperfluidToken(address(0)), trustedForwarders[i]);
        }
    }

    function _requireAuthorised(ISuperfluid host)
        internal view override
    {
        require(host == _host, "TestGovernance: unrecogonized host");
        require(owner() == _msgSender(), "TestGovernance: only owner is authorized");
    }
}
