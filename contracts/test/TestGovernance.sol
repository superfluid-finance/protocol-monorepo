// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { Ownable } from "../access/Ownable.sol";
import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperfluidGovernance
} from "../interfaces/superfluid/ISuperfluid.sol";


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
        _owner = msg.sender;
        _rewardAddress = rewardAddress;
        _liquidationPeriod = liquidationPeriod;
    }

    function setRewardAddress(
        address rewardAddress
    )
        external
        onlyOwner
    {
        _rewardAddress = rewardAddress;
    }

    function setLiquidationPeriod(uint256 liquidationPeriod) external {
        _liquidationPeriod = liquidationPeriod;
    }

    function registerAgreementClass(address host, ISuperAgreement agreementClass)
        external override
    {
        ISuperfluid(host).registerAgreementClass(agreementClass);
    }

    function updateAgreementClass(address host, ISuperAgreement agreementClass)
        external override
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
