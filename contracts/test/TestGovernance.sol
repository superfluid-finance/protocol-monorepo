// SPDX-License-Identifier: MIT
pragma solidity 0.7.6;

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

    function updateContracts(
        ISuperfluid host,
        address hostNewLogic,
        address[] calldata agreementClassNewLogics,
        address superTokenFactoryNewLogic
    )
        external override
        onlyOwner
    {
        if (hostNewLogic != address(0)) {
            UUPSProxiable(address(host)).updateCode(hostNewLogic);
        }
        for (uint i = 0; i < agreementClassNewLogics.length; ++i) {
            host.updateAgreementClass(ISuperAgreement(agreementClassNewLogics[i]));
        }
        if (superTokenFactoryNewLogic != address(0)) {
            host.updateSuperTokenFactory(ISuperTokenFactory(superTokenFactoryNewLogic));
        }
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
