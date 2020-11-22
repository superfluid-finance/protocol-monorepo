// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperToken,
    ISuperfluidGovernance
} from "../interfaces/superfluid/ISuperfluid.sol";

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

    function registerAgreementClass(
        address host,
        ISuperAgreement agreementClass
    )
        external override
        onlyOwner
    {
        ISuperfluid(host).registerAgreementClass(agreementClass);
    }

    function replaceGovernance(
        address host,
        ISuperfluidGovernance newGov
    )
        external override
        onlyOwner
    {
        ISuperfluid(host).replaceGovernance(newGov);
    }

    function setSuperTokenLogic(
        address host,
        address logic
    )
        external override
        onlyOwner
    {
        ISuperfluid(host).setSuperTokenLogic(ISuperToken(logic));
    }

    function updateAgreementClass(
        address host,
        ISuperAgreement agreementClass
    )
        external override
        onlyOwner
    {
        ISuperfluid(host).updateAgreementClass(agreementClass);
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
