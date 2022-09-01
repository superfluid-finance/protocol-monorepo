// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.14;

import {
    ISuperTokenFactory,
    ISuperToken,
    IERC20,
    ERC20WithTokenInfo,
    SuperfluidErrors
} from "../interfaces/superfluid/ISuperTokenFactory.sol";

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import { SuperToken } from "../superfluid/SuperToken.sol";

import {
    FullUpgradableSuperTokenProxy
} from "./FullUpgradableSuperTokenProxy.sol";

import { Address } from "@openzeppelin/contracts/utils/Address.sol";
import { Create2 } from "@openzeppelin/contracts/utils/Create2.sol";

abstract contract SuperTokenFactoryBase is UUPSProxiable, ISuperTokenFactory {
    ISuperfluid internal immutable _host;

    ISuperToken internal _superTokenLogic;

    constructor(ISuperfluid host) {
        _host = host;
    }

    /// @dev ISuperTokenFactory.getHost implementation
    function getHost()
        external
        view
        override(ISuperTokenFactory)
        returns (address host)
    {
        return address(_host);
    }

    /**************************************************************************
     * UUPSProxiable
     **************************************************************************/
    function initialize()
        external
        override
        initializer // OpenZeppelin Initializable
    {
        _updateSuperTokenLogic();
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return
            keccak256(
                "org.superfluid-finance.contracts.SuperTokenFactory.implementation"
            );
    }

    function updateCode(address newAddress) external override {
        if (msg.sender != address(_host)) {
            revert SuperfluidErrors.ONLY_HOST(
                SuperfluidErrors.SUPER_TOKEN_FACTORY_ONLY_HOST
            );
        }
        _updateCodeAddress(newAddress);
        _updateSuperTokenLogic();
    }

    function _updateSuperTokenLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _superTokenLogic = SuperToken(this.createSuperTokenLogic(_host));
        UUPSProxiable(address(_superTokenLogic)).castrate();
        emit SuperTokenLogicCreated(_superTokenLogic);
    }

    /**************************************************************************
     * ISuperTokenFactory
     **************************************************************************/
    function getSuperTokenLogic() external view override returns (ISuperToken) {
        return _superTokenLogic;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external
        virtual
        returns (address logic);

    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    ) public override returns (ISuperToken superToken) {
        if (address(underlyingToken) == address(0)) {
            revert SuperfluidErrors.ZERO_ADDRESS(
                SuperfluidErrors.SUPER_TOKEN_FACTORY_ZERO_ADDRESS
            );
        }

        if (upgradability == Upgradability.NON_UPGRADABLE) {
            superToken = ISuperToken(this.createSuperTokenLogic(_host));
        } else if (upgradability == Upgradability.SEMI_UPGRADABLE) {
            UUPSProxy proxy = new UUPSProxy();
            // initialize the wrapper
            proxy.initializeProxy(address(_superTokenLogic));
            superToken = ISuperToken(address(proxy));
        }
        /* if (type == Upgradability.FULL_UPGRADABE) */
        else {
            FullUpgradableSuperTokenProxy proxy = new FullUpgradableSuperTokenProxy();
            proxy.initialize();
            superToken = ISuperToken(address(proxy));
        }

        // initialize the token
        superToken.initialize(
            underlyingToken,
            underlyingDecimals,
            name,
            symbol
        );

        emit SuperTokenCreated(superToken);
    }

    function createERC20Wrapper(
        ERC20WithTokenInfo underlyingToken,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    ) external override returns (ISuperToken superToken) {
        return
            createERC20Wrapper(
                underlyingToken,
                underlyingToken.decimals(),
                upgradability,
                name,
                symbol
            );
    }

    function initializeCustomSuperToken(address customSuperTokenProxy)
        external
        override
    {
        // odd solidity stuff..
        // NOTE payable necessary because UUPSProxy has a payable fallback function
        address payable a = payable(address(uint160(customSuperTokenProxy)));
        UUPSProxy(a).initializeProxy(address(_superTokenLogic));

        emit CustomSuperTokenCreated(ISuperToken(customSuperTokenProxy));
    }
}

// splitting this off because the contract is getting bigger
contract SuperTokenFactoryHelper {
    function create(ISuperfluid host) external returns (address logic) {
        return address(new SuperToken(host));
    }
}

contract SuperTokenFactory is SuperTokenFactoryBase {
    SuperTokenFactoryHelper private immutable _helper;

    constructor(ISuperfluid host, SuperTokenFactoryHelper helper)
        SuperTokenFactoryBase(host)
    // solhint-disable-next-line no-empty-blocks
    {
        _helper = helper;
    }

    function createSuperTokenLogic(ISuperfluid host)
        external
        override
        returns (address logic)
    {
        return _helper.create(host);
    }
}
