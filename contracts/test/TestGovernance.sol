// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";

contract TestGovernance is ISuperfluidGovernance {

    address public governor;

    struct GovernanceConfig {
        address rewardAddress;
        uint16 minimalDeposit;
        uint16 period;
    }

    GovernanceConfig private _defaultConfig;

    constructor(
        address rewardAddress,
        uint16 minimalDeposit,
        uint16 period
    )
        public
    {
        governor = msg.sender;
        _defaultConfig = GovernanceConfig(rewardAddress, minimalDeposit, period);
    }

    function getRewardAddress(
        address
    )
        external
        view
        override
        returns(address rewardAddress)
    {
        return _defaultConfig.rewardAddress;
    }

    function getMinimalDeposit(
        address
    )
        external
        view
        override
        returns(uint16 minimalBalance)
    {
        return _defaultConfig.minimalDeposit;
    }

    function getLiquidationPeriod(
        address
    )
        external
        view
        override
        returns(uint16 period)
    {
        return _defaultConfig.period;
    }
}
