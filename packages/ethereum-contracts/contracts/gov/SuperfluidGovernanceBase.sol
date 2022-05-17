// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

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
 * @title Base superfluid governance implementation
 * @author Superfluid
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

    function batchUpdateSuperTokenMinimumDeposit(
        ISuperfluid host,
        ISuperToken[] calldata tokens,
        uint256[] calldata minimumDeposits
    ) external {
        require(tokens.length == minimumDeposits.length, "SFGov: arrays are not the same length");
        for (uint i = 0; i < minimumDeposits.length; ++i) {
            setSuperTokenMinimumDeposit(
                host,
                tokens[i],
                minimumDeposits[i]
            );
        }
    }

    event ConfigChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bytes32 key,
		bool isKeySet,
        uint256 value);

    function setConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key,
        address value
    )
        external override
    {
        _setConfig(host, superToken, key, value);
    }

    function setConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key,
        uint256 value
    )
        external override
    {
        _setConfig(host, superToken, key, value);
    }

    function clearConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        bytes32 key
    )
        external override
    {
        _clearConfig(host, superToken, key);
    }

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
        return address(uint160(v.value));
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

    // CFAv1 liquidationPeriod (DEPRECATED BY PPPConfigurationChanged)
    event CFAv1LiquidationPeriodChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        uint256 liquidationPeriod);

    // CFAv1 PPPConfiguration - Liquidation Period + Patrician Period
    event PPPConfigurationChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        uint256 liquidationPeriod,
        uint256 patricianPeriod);

    function getPPPConfig(
        ISuperfluid host,
        ISuperfluidToken superToken
    ) public view
        returns (uint256 liquidationPeriod, uint256 patricianPeriod)
        {
            uint256 pppConfig = getConfigAsUint256(
                host,
                superToken,
                SuperfluidGovernanceConfigs.CFAV1_PPP_CONFIG_KEY
            );
            (liquidationPeriod, patricianPeriod) = SuperfluidGovernanceConfigs.decodePPPConfig(pppConfig);
        }

    function setPPPConfig(
        ISuperfluid host,
        ISuperfluidToken superToken,
        uint256 liquidationPeriod,
        uint256 patricianPeriod
    )
        public
    {
        require(liquidationPeriod > patricianPeriod
            && liquidationPeriod < type(uint32).max
            && patricianPeriod < type(uint32).max,
            "SFGov: Invalid liquidationPeriod or patricianPeriod"
        );
        emit PPPConfigurationChanged(host, superToken, true, liquidationPeriod, patricianPeriod);
        uint256 value = (uint256(liquidationPeriod) << 32) | uint256(patricianPeriod);
        return _setConfig(
            host,
            superToken,
            SuperfluidGovernanceConfigs.CFAV1_PPP_CONFIG_KEY,
            value
        );
    }

    function clearPPPConfig(
        ISuperfluid host,
        ISuperfluidToken superToken
    ) public {
        emit PPPConfigurationChanged(host, superToken, false, 0, 0);
        return _clearConfig(host, superToken, SuperfluidGovernanceConfigs.CFAV1_PPP_CONFIG_KEY);
    }
    event SuperTokenMinimumDepositChanged(
        ISuperfluid indexed host,
        ISuperfluidToken indexed superToken,
        bool isKeySet,
        uint256 minimumDeposit
    );

    function getSuperTokenMinimumDeposit(
        ISuperfluid host,
        ISuperfluidToken superToken
    ) public view
    returns (uint256 value)
    {
        return getConfigAsUint256(host, superToken,
            SuperfluidGovernanceConfigs.SUPERTOKEN_MINIMUM_DEPOSIT_KEY);
    }

    function setSuperTokenMinimumDeposit(
        ISuperfluid host,
        ISuperfluidToken superToken,
        uint256 value
    ) public {
        emit SuperTokenMinimumDepositChanged(host, superToken, true, value);
        return _setConfig(host, superToken, SuperfluidGovernanceConfigs.SUPERTOKEN_MINIMUM_DEPOSIT_KEY, value);
    }

    function clearSuperTokenMinimumDeposit(
        ISuperfluid host,
        ISuperToken superToken
    ) public
    {
        emit SuperTokenMinimumDepositChanged(host, superToken, false, 0);
        return _clearConfig(host, superToken, SuperfluidGovernanceConfigs.SUPERTOKEN_MINIMUM_DEPOSIT_KEY);
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
