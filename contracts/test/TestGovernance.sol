// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    ISuperfluidGovernance
} from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

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
        UUPSProxiable(address(host)).updateCode(newCode);
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

    function updateSuperTokenFactory(
        ISuperfluid host,
        address newFactory
    )
        external override
        onlyOwner
    {
        host.updateSuperTokenFactory(ISuperTokenFactory(newFactory));
    }

    function updateSuperTokenLogic(
        ISuperfluid host,
        ISuperToken token
    )
        external override
        onlyOwner
    {
        host.updateSuperTokenLogic(token);
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
