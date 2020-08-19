// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";

contract TestGovernance is ISuperfluidGovernance {

    address public governor;

    struct GovernanceConfig {
        address rewardAddress;
        uint16 minimalDeposit;
        uint16 period;
        uint64 maxGasCallback;
        uint64 maxGasApp;
        address superfluid;
    }

    GovernanceConfig private _defaultConfig;

    constructor(
        address rewardAddress,
        uint16 minimalDeposit,
        uint16 period,
        uint64 maxGasCallback,
        uint64 maxGasApp,
        address superfluid
    )
    public
    {
        governor = msg.sender;
        _defaultConfig = GovernanceConfig(
            rewardAddress,
            minimalDeposit,
            period,
            maxGasCallback,
            maxGasApp,
            superfluid
        );
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

    function getMaxGasCallback() external view override returns(uint64) {
        return _defaultConfig.maxGasCallback;
    }

    function getMaxGasApp() external view override returns(uint64) {
        return _defaultConfig.maxGasApp;
    }

    function getSuperfluid() external view override returns(address) {
        return _defaultConfig.superfluid;
    }
}
