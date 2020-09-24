// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;

import { Ownable } from "../access/Ownable.sol";
import {
    ISuperfluid,
    ISuperfluidGovernance
} from "../interfaces/ISuperfluid.sol";

contract TestGovernance is
    Ownable,
    ISuperfluidGovernance
{
    address private _rewardAddress;
    uint256 private _liquidationPeriod;
    address[] private _agreementList;
    mapping (address => uint) private _agreementListedFlags;

    constructor(
        address rewardAddress,
        uint256 liquidationPeriod
    )
    {
        _owner = msg.sender;
        _rewardAddress = rewardAddress;
        _liquidationPeriod = liquidationPeriod;
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

    function addAgreement(address agreementClass) external onlyOwner override {
        _agreementList.push(agreementClass);
        _agreementListedFlags[agreementClass] = _agreementList.length;
    }

    function isAgreementListed(address agreementClass) external override view returns(bool yes) {
        return _agreementListedFlags[agreementClass] > 0;
    }
}
