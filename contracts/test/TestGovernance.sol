pragma solidity ^0.6.0;

import { ISuperfluidGovernance } from "../interface/ISuperfluidGovernance.sol";

contract TestGovernance is ISuperfluidGovernance {

    address public Governor;

    struct GovernanceConfig {
        address rewardAddress;
        uint256 minimalBalance;
        uint16 period;
    }

    mapping(address => GovernanceConfig) private _configs;

    constructor(
        address underlying,
        address rewardAddress,
        uint256 minimalBalance,
        uint16 period
    )
        public
    {
        Governor = msg.sender;
        _configs[underlying] = GovernanceConfig(rewardAddress, minimalBalance, period);
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

    function getMinimalBalance(
        address underlying
    )
        external
        view
        override
        returns(uint256 minimalBalance)
    {
        return _configs[underlying].minimalBalance;
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

    function getGovParams(
        address underlying
    )
        external
        view
        override
        returns(address, uint256, uint16)
    {
        return (_configs[underlying].rewardAddress,
                _configs[underlying].minimalBalance,
                _configs[underlying].period
        );
    }
}
