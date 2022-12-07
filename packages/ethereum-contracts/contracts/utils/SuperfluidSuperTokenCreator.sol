// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

import { SETHProxy } from "../tokens/SETH.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";
import { IPureSuperToken } from "../interfaces/tokens/IPureSuperToken.sol";
import { ISETH } from "../interfaces/tokens/ISETH.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { TestToken } from "./TestToken.sol";

contract SuperfluidSuperTokenCreator {
    string public constant RESOLVER_BASE_SUPER_TOKEN_KEY = "supertokens.test.";
    string public constant RESOLVER_BASE_TOKEN_KEY = "tokens.test.";

    SuperTokenFactory public superTokenFactory;
    TestResolver public resolver;

    constructor(SuperTokenFactory _superTokenFactory, TestResolver _resolver) {
        superTokenFactory = _superTokenFactory;
        resolver = _resolver;
    }

    /// @notice Deploys an ERC20 and a Wrapper Super Token for the ERC20 and lists both in the resolver
    /// @dev SuperToken name and symbol format: `Super ${_underlyingSymbol}` and `${_underlyingSymbol}x`, respectively
    /// @param _underlyingName The underlying token name
    /// @param _underlyingSymbol The token symbol
    /// @return underlyingToken and superToken
    function deployWrapperSuperToken(
        string calldata _underlyingName,
        string calldata _underlyingSymbol,
        uint8 _decimals,
        uint256 _mintLimit
    ) external returns (TestToken underlyingToken, SuperToken superToken) {
        underlyingToken = new TestToken(
            _underlyingName,
            _underlyingSymbol,
            _decimals,
            _mintLimit
        );
        superToken = SuperToken(
            address(
                superTokenFactory.createERC20Wrapper(
                    ERC20WithTokenInfo(address(underlyingToken)),
                    ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
                    string.concat("Super ", _underlyingSymbol),
                    string.concat(_underlyingSymbol, "x")
                )
            )
        );

        // list underlying token in resolver
        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_TOKEN_KEY, underlyingToken.symbol()),
            address(underlyingToken)
        );

        // list super token in resolver
        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, superToken.symbol()),
            address(superToken)
        );
    }

    /// @notice Deploys a Native Asset Super Token and lists it in the resolver
    /// @dev e.g. ETHx, MATICx, AVAXx, etc. The underlying is the Native Asset.
    /// @param _name The token name
    /// @param _symbol The token symbol
    /// @return nativeAssetSuperToken
    function deployNativeAssetSuperToken(
        string calldata _name,
        string calldata _symbol
    ) external returns (ISETH nativeAssetSuperToken) {
        SETHProxy sethProxy = new SETHProxy();
        nativeAssetSuperToken = ISETH(address(sethProxy));
        superTokenFactory.initializeCustomSuperToken(address(sethProxy));
        nativeAssetSuperToken.initialize(
            IERC20(address(0)),
            18,
            _name,
            _symbol
        );

        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, _symbol),
            address(nativeAssetSuperToken)
        );
    }

    /// @notice Deploys a Pure Super Token and lists it in the resolver
    /// @dev A Pure Super Token cannot be downgraded, which is why we specify the initial supply on creation
    /// @param _name The token name
    /// @param _symbol The token symbol
    /// @param _initialSupply The initial token supply of the pure super token
    /// @return pureSuperToken
    function deployPureSuperToken(
        string calldata _name,
        string calldata _symbol,
        uint256 _initialSupply
    ) external returns (IPureSuperToken pureSuperToken) {
        PureSuperToken pureSuperTokenProxy = new PureSuperToken();
        superTokenFactory.initializeCustomSuperToken(
            address(pureSuperTokenProxy)
        );
        pureSuperTokenProxy.initialize(_name, _symbol, _initialSupply);

        pureSuperToken = IPureSuperToken(address(pureSuperTokenProxy));

        pureSuperToken.transfer(msg.sender, _initialSupply);

        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, _symbol),
            address(pureSuperToken)
        );
    }

    function _handleResolverList(
        bool _listOnResolver,
        string memory _resolverKey,
        address _superTokenAddress
    ) internal {
        if (_listOnResolver) {
            resolver.set(_resolverKey, address(_superTokenAddress));
        }
    }
}