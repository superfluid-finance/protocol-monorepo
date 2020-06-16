// SPDX-License-Identifier: MIT
pragma solidity ^0.6.0;

import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";

contract TestGovernance is ISuperfluidGovernance {

    address public Governor;

    struct GovernanceConfig {
        address rewardAddress;
        uint16 minimalDeposit;
        uint16 period;
    }

    mapping(address => GovernanceConfig) private _configs;

    constructor(
        address underlying,
        address rewardAddress,
        uint16 minimalDeposit,
        uint16 period
    )
        public
    {
        Governor = msg.sender;
        _configs[underlying] = GovernanceConfig(rewardAddress, minimalDeposit, period);
    }

    function getRewardAddress(
        address underlying
    )
        external
        view
        override
        returns(address rewardAddress)
    {
        return _configs[underlying].rewardAddress;
    }

    function getMinimalDeposit(
        address underlying
    )
        external
        view
        override
        returns(uint16 minimalBalance)
    {
        return _configs[underlying].minimalDeposit;
    }

    function getLiquidationPeriod(
        address underlying
    )
        external
        view
        override
        returns(uint16 period)
    {
        return _configs[underlying].period;
    }
}
