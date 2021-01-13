// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperTokenFactory,
    ISuperToken,
    IERC20,
    ERC20WithTokenInfo
} from "../interfaces/superfluid/ISuperTokenFactory.sol";

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";
import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";

import { SuperToken } from "../superfluid/SuperToken.sol";
import { FullUpgradableSuperTokenProxy } from "../tokens/FullUpgradableSuperTokenProxy.sol";

import { Address } from "@openzeppelin/contracts/utils/Address.sol";
import { Create2 } from "@openzeppelin/contracts/utils/Create2.sol";


abstract contract SuperTokenFactoryBase is
    UUPSProxiable,
    ISuperTokenFactory
{

    ISuperfluid immutable internal _host;

    ISuperToken internal _superTokenLogic;

    constructor(
        ISuperfluid host
    ) {
        _host = host;
    }

    /// @dev ISuperTokenFactory.getHost implementation
    function getHost()
       external view
       override(ISuperTokenFactory)
       returns(address host)
    {
       return address(_host);
    }

    /**************************************************************************
    * UUPSProxiable
    **************************************************************************/
    function initialize()
        external override
        initializer // OpenZeppelin Initializable
    {
        _updateSuperTokenLogic();
    }

    function proxiableUUID() public pure override returns (bytes32) {
        return keccak256("org.superfluid-finance.contracts.SuperTokenFactory.implementation");
    }

    function updateCode(address newAddress) external override {
        require(msg.sender == address(_host), "only host can update code");
        _updateCodeAddress(newAddress);
        _updateSuperTokenLogic();
    }

    function _updateSuperTokenLogic() private {
        // use external call to trigger the new code to update the super token logic contract
        _superTokenLogic = SuperToken(this.createSuperTokenLogic(_host));
        emit SuperTokenLogicCreated(_superTokenLogic);
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

    function createSuperTokenLogic(ISuperfluid host) external virtual returns (address logic);

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
            token = ISuperToken(this.createSuperTokenLogic(_host));
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

    function initializeCustomSuperToken(
        address customSuperTokenProxy
    )
        external override
    {
        // odd solidity stuff..
        address payable a = address(uint160(customSuperTokenProxy));
        UUPSProxy(a).initializeProxy(address(_superTokenLogic));
    }

}

contract SuperTokenFactory is SuperTokenFactoryBase
{

    constructor(
        ISuperfluid host
    )
        SuperTokenFactoryBase(host)
        // solhint-disable-next-line no-empty-blocks
    {
    }

    function createSuperTokenLogic(ISuperfluid host)
        external override
        returns (address logic)
    {
        return address(new SuperToken(host));
    }
}
