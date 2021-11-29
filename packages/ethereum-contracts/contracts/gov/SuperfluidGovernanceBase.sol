// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperfluid,
    ISuperAgreement,
    ISuperfluidToken,
    ISuperToken,
    ISuperTokenFactory,
    ISuperfluidGovernance,
    SuperfluidGovernanceConfigs
} from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";


/**
 * @dev Base superfluid governance implementation
 */
abstract contract SuperfluidGovernanceBase is ISuperfluidGovernance
{
    struct Value {
        bool set;
        uint256 value;
    }

    // host => superToken => config
    mapping (address => mapping (address => mapping (bytes32 => Value))) internal _configs;

    /**************************************************************************
    /* ISuperfluidGovernance interface
    /*************************************************************************/

    function replaceGovernance(
        ISuperfluid host,
        address newGov
    )
        external override
        onlyAuthorized(host)
    {
        host.replaceGovernance(ISuperfluidGovernance(newGov));
    }

    function registerAgreementClass(
        ISuperfluid host,
        address agreementClass
    )
        external override
        onlyAuthorized(host)
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
        onlyAuthorized(host)
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

    function batchUpdateSuperTokenLogic(
        ISuperfluid host,
        ISuperToken[] calldata tokens
    )
        external override
        onlyAuthorized(host)
    {
        for (uint i = 0; i < tokens.length; ++i) {
            host.updateSuperTokenLogic(tokens[i]);
        }
    }

    event ConfigChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bytes32 key,
		bool isKeySet,
        uint256 value);

    function _setConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key,
        address value
    )
        internal
        onlyAuthorized(host)
    {
        emit ConfigChanged(host, superToken, key, true, uint256(uint160(value)));
        _configs[address(host)][address(superToken)][key] = Value(true, uint256(uint160(value)));
    }

    function _setConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key,
        uint256 value
    )
        internal
        onlyAuthorized(host)
    {
        emit ConfigChanged(host, superToken, key, true, value);
        _configs[address(host)][address(superToken)][key] = Value(true, value);
    }

    function _clearConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key
    )
        internal
        onlyAuthorized(host)
    {
        emit ConfigChanged(host, superToken, key, false, 0);
        _configs[address(host)][address(superToken)][key] = Value(false, 0);
    }

    function getConfigAsAddress(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key
    )
        public view override
        returns(address value)
    {
        Value storage v = _configs[address(host)][address(superToken)][key];
        if (!v.set) {
            // fallback to default config
            v =  _configs[address(host)][address(0)][key];
        }
        return address(int160(v.value));
    }

    function getConfigAsUint256(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key
    )
        public view override
        returns(uint256 period)
    {
        Value storage v = _configs[address(host)][address(superToken)][key];
        if (!v.set) {
            // fallback to default config
            v =  _configs[address(host)][address(0)][key];
        }
        return v.value;
    }

    /**************************************************************************
    /* Known Configurations
    /*************************************************************************/

    // Superfluid rewardAddress
    event RewardAddressChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        address rewardAddress);

    function getRewardAddress(
        ISuperfluid host,
        ISuperfluidToken superToken
    )
        public view returns (address)
    {
        return getConfigAsAddress(
            host, superToken,
            SuperfluidGovernanceConfigs.SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY);
    }

    function setRewardAddress(
        ISuperfluid host,
        ISuperfluidToken superToken,
        address rewardAddress
    )
        public
    {
        emit RewardAddressChanged(host, superToken, true, rewardAddress);
        return _setConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY,
            rewardAddress);
    }

    function clearRewardAddress(
        ISuperfluid host,
        ISuperfluidToken superToken
    )
        public
    {
        emit RewardAddressChanged(host, superToken, false, address(0));
        _clearConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.SUPERFLUID_REWARD_ADDRESS_CONFIG_KEY);
    }

    // CFAv1 liquidationPeriod
    event CFAv1LiquidationPeriodChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        uint256 liquidationPeriod);

    function getCFAv1LiquidationPeriod(
        ISuperfluid host,
        ISuperfluidToken superToken
    )
        public view
        returns (uint256 value)
    {
        return getConfigAsUint256(
            host, superToken,
            SuperfluidGovernanceConfigs.CFAv1_LIQUIDATION_PERIOD_CONFIG_KEY);
    }

    function setCFAv1LiquidationPeriod(
        ISuperfluid host,
        ISuperfluidToken superToken,
        uint256 value
    )
        public
    {
        emit CFAv1LiquidationPeriodChanged(host, superToken, true, value);
        return _setConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.CFAv1_LIQUIDATION_PERIOD_CONFIG_KEY,
            value);
    }

    function clearCFAv1LiquidationPeriod(
        ISuperfluid host,
        ISuperfluidToken superToken
    )
        public
    {
        emit CFAv1LiquidationPeriodChanged(host, superToken, false, 0);
        _clearConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.CFAv1_LIQUIDATION_PERIOD_CONFIG_KEY);
    }

    // trustedForwarder
    event TrustedForwarderChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        address forwarder,
        bool enabled);

    function isTrustedForwarder(
        ISuperfluid host,
        ISuperfluidToken superToken,
        address forwarder
    )
        public view
        returns (bool)
    {
        return getConfigAsUint256(
            host, superToken,
            SuperfluidGovernanceConfigs.getTrustedForwarderConfigKey(forwarder)) == 1;
    }

    function enableTrustedForwarder(
        ISuperfluid host,
        ISuperfluidToken superToken,
        address forwarder
    )
        public
    {
        _setConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.getTrustedForwarderConfigKey(forwarder),
            1);
        emit TrustedForwarderChanged(host, superToken, true, forwarder, true);
    }

    function disableTrustedForwarder(
        ISuperfluid host,
        ISuperfluidToken superToken,
        address forwarder
    )
        public
    {
        _setConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.getTrustedForwarderConfigKey(forwarder),
            0);
        emit TrustedForwarderChanged(host, superToken, true, forwarder, false);
    }

    function clearTrustedForwarder(
        ISuperfluid host,
        ISuperfluidToken superToken,
        address forwarder
    )
        public
    {
        emit TrustedForwarderChanged(host, superToken, false, forwarder, false);
        return _clearConfig(
            host, superToken,
            SuperfluidGovernanceConfigs.getTrustedForwarderConfigKey(forwarder));
    }

    /**
     * @dev Whitelist a new app using a onetime key
     * @param key is a deployer specific hash key which can be used once to register an app
     *
     * NOTE:
     * To generate the key, use the SuperfluidGovernanceConfigs.getAppRegistrationConfigKey
     * offchain.
     */
    function whiteListNewApp(
        ISuperfluid host,
        bytes32 key
    )
        external
    {
        _setConfig(host, ISuperfluidToken(address(0)), key, 1);
    }

    /**
     * @dev tells if the given factory is authorized to register apps
     */
    function isAuthorizedAppFactory(
        ISuperfluid host,
        address factory
    )
        public view
        returns (bool)
    {
        return getConfigAsUint256(
            host, ISuperfluidToken(address(0)),
            SuperfluidGovernanceConfigs.getAppFactoryConfigKey(factory)) == 1;
    }

    /**
     * @dev allows the given factory to register new apps without requiring onetime keys
     * @param factory must be an initialized contract
     */
    function authorizeAppFactory(
        ISuperfluid host,
        address factory
    )
        public
    {
        // check if contract
        {
            uint256 cs;
            // solhint-disable-next-line no-inline-assembly
            assembly { cs := extcodesize(factory) }
            require(cs > 0, "SFGov: factory must be a contract");
        }

        _setConfig(
            host, ISuperfluidToken(address(0)),
            SuperfluidGovernanceConfigs.getAppFactoryConfigKey(factory),
            1);
    }

    /**
     * @dev withdraws authorization from a factory to register new apps.
     * Doesn't affect apps previously registered by the factory.
     */
    function unauthorizeAppFactory(
        ISuperfluid host,
        address factory
    )
        public
    {
        _clearConfig(
            host, ISuperfluidToken(address(0)),
            SuperfluidGovernanceConfigs.getAppFactoryConfigKey(factory));
    }

    // TODO: would like to use virtual modifier, but solhint doesn't like it atm
    modifier onlyAuthorized(ISuperfluid host) {
        _requireAuthorised(host);
        _;
    }

    function _requireAuthorised(ISuperfluid host) internal view virtual;
}
