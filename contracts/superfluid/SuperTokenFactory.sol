// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import {
    ISuperTokenFactory,
    ISuperToken,
    IERC20,
    ERC20WithTokenInfo
} from "../interfaces/superfluid/ISuperTokenFactory.sol";

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import { FullUpgradableSuperTokenProxy } from "../tokens/FullUpgradableSuperTokenProxy.sol";

import { Address } from "@openzeppelin/contracts/utils/Address.sol";
import { Create2 } from "@openzeppelin/contracts/utils/Create2.sol";


abstract contract SuperTokenFactoryBase is
    UUPSProxiable,
    ISuperTokenFactory
{

    ISuperfluid internal _host;

    ISuperToken internal _superTokenLogic;

    /**************************************************************************
    * UUPSProxiable
    **************************************************************************/
    function initialize()
        external override
        initializer // OpenZeppelin Initializable
    {
        _host = ISuperfluid(msg.sender);
        _superTokenLogic = SuperToken(this.createSuperTokenLogic());
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperTokenFactory.implementation");
    }

    function updateCode(address newAddress) external override {
        require(msg.sender == address(_host), "only host can update code");
        _updateCodeAddress(newAddress);
        // use external call to trigger the new code to update the super token logic contract
        _superTokenLogic = SuperToken(this.createSuperTokenLogic());
    }

    /**************************************************************************
    * ISuperTokenFactory
    **************************************************************************/
    function getSuperTokenLogic()
        external view override
        returns (ISuperToken)
    {
        return _superTokenLogic;
    }

    function createSuperTokenLogic() external virtual returns (address logic);

    function createERC20Wrapper(
        IERC20 underlyingToken,
        uint8 underlyingDecimals,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        public override
    {
        require(address(underlyingToken) != address(0), "SuperTokenFactory: zero address");

        ISuperToken token;
        if (upgradability == Upgradability.NON_UPGRADABLE) {
            token = ISuperToken(this.createSuperTokenLogic());
        } else if (upgradability == Upgradability.SEMI_UPGRADABLE) {
            UUPSProxy proxy = new UUPSProxy();
            // initialize the wrapper
            proxy.initializeProxy(address(_superTokenLogic));
            token = ISuperToken(address(proxy));
        } else /* if (type == Upgradability.FULL_UPGRADABE) */ {
            FullUpgradableSuperTokenProxy proxy = new FullUpgradableSuperTokenProxy();
            proxy.initialize();
            token = ISuperToken(address(proxy));
        }

        // initialize the token
        token.initialize(
            _host,
            underlyingToken,
            underlyingDecimals,
            name,
            symbol
        );

        emit SuperTokenCreated(token);
    }

    function createERC20Wrapper(
        ERC20WithTokenInfo underlyingToken,
        Upgradability upgradability,
        string calldata name,
        string calldata symbol
    )
        external override
    {
        createERC20Wrapper(
            underlyingToken,
            underlyingToken.decimals(),
            upgradability,
            name,
            symbol
        );
    }

}

contract SuperTokenFactory is SuperTokenFactoryBase
{
    function createSuperTokenLogic()
        external override
        returns (address logic)
    {
        return address(new SuperToken());
    }
}
