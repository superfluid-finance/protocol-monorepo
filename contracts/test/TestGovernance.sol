// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperToken,
    ISuperfluidGovernance
} from "../interfaces/superfluid/ISuperfluid.sol";

import { Proxiable } from "../upgradability/Proxiable.sol";

import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";


contract TestGovernance is
    Ownable,
    ISuperfluidGovernance
{
    address private _rewardAddress;
    uint256 private _liquidationPeriod;

    constructor(
        address rewardAddress,
        uint256 liquidationPeriod
    )
    {
        _rewardAddress = rewardAddress;
        _liquidationPeriod = liquidationPeriod;

        transferOwnership(msg.sender);
    }

    /**************************************************************************
    /* Configurations
    /*************************************************************************/

    function setRewardAddress(
        address rewardAddress
    )
        external
        onlyOwner
    {
        _rewardAddress = rewardAddress;
    }

    function setLiquidationPeriod(uint256 liquidationPeriod)
        external
        onlyOwner
    {
        _liquidationPeriod = liquidationPeriod;
    }

    /**************************************************************************
    /* ISuperfluidGovernance
    /*************************************************************************/

    function updateHostCode(
        ISuperfluid host,
        address newCode
    )
        external override
        onlyOwner
    {
        Proxiable(address(host)).updateCode(newCode);
    }

    function replaceGovernance(
        ISuperfluid host,
        address newGov
    )
        external override
        onlyOwner
    {
        host.replaceGovernance(ISuperfluidGovernance(newGov));
    }

    function setSuperTokenLogic(
        ISuperfluid host,
        address newLogic
    )
        external override
        onlyOwner
    {
        host.setSuperTokenLogic(ISuperToken(newLogic));
    }

    function registerAgreementClass(
        ISuperfluid host,
        address agreementClass
    )
        external override
        onlyOwner
    {
        host.registerAgreementClass(ISuperAgreement(agreementClass));
    }

    function updateAgreementClass(
        ISuperfluid host,
        address agreementClass
    )
        external override
        onlyOwner
    {
        host.updateAgreementClass(ISuperAgreement(agreementClass));
    }

    function getRewardAddress(
        ISuperfluidToken /* superToken */
    )
        external view override
        returns(address rewardAddress)
    {
        return _rewardAddress;
    }

    function getLiquidationPeriod(
        ISuperfluidToken /* superToken */
    )
        external view override
        returns(uint256 period)
    {
        return _liquidationPeriod;
    }
}
