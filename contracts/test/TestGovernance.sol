// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

import { Ownable } from "../access/Ownable.sol";
import {
    ISuperfluid,
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

    function addAgreement(address host, address agreementClass)
        external
        override
    {
        ISuperfluid(host).addAgreement(agreementClass);
    }

    function getRewardAddress(
        address /* superToken */
    )
        external
        view
        override
        returns(address rewardAddress)
    {
        return _rewardAddress;
    }

    function setRewardAddress(
        address rewardAddress
    )
        external
        onlyOwner
    {
        _rewardAddress = rewardAddress;
    }

    function getLiquidationPeriod(
        address /* superToken */
    )
        external
        view
        override
        returns(uint256 period)
    {
        return _liquidationPeriod;
    }

}
