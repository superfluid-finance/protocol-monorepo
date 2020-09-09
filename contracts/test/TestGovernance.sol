// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;

import { ISuperfluidGovernance } from "../interfaces/ISuperfluidGovernance.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";

contract TestGovernance is ISuperfluidGovernance {

    address public governor;

    struct GovernanceConfig {
        address rewardAddress;
        uint16 period;
        uint64 maxGasCallback;
        uint64 maxGasApp;
        address superfluid;
    }

    GovernanceConfig private _defaultConfig;

    constructor(
        address rewardAddress,
        uint16 period,
        uint64 maxGasCallback,
        uint64 maxGasApp,
        address superfluid
    )
    {
        governor = msg.sender;
        _defaultConfig = GovernanceConfig(
            rewardAddress,
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

    function addAgreement(address agreement) external onlyGovernator override {
        ISuperfluid(_defaultConfig.superfluid).addAgreement(agreement);
    }

    modifier onlyGovernator {
        require(msg.sender == governor, "Gov: Not allowed");
        _;
    }
}
