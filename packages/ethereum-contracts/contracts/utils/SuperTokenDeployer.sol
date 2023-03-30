// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ConstantOutflowNFT } from "../superfluid/ConstantOutflowNFT.sol";
import { ConstantInflowNFT } from "../superfluid/ConstantInflowNFT.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import {
    IConstantOutflowNFT,
    IConstantInflowNFT,
    IPoolAdminNFT,
    IPoolMemberNFT
} from "../interfaces/superfluid/ISuperToken.sol";
import { IPureSuperToken } from "../interfaces/tokens/IPureSuperToken.sol";
import { ISETH } from "../interfaces/tokens/ISETH.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";
import { SETHProxy } from "../tokens/SETH.sol";
import { Superfluid } from "../superfluid/Superfluid.sol";
import { SuperToken, ISuperToken } from "../superfluid/SuperToken.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { TestGovernance } from "./TestGovernance.sol";
import { TestResolver } from "./TestResolver.sol";
import { TestToken } from "./TestToken.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import {
    SuperfluidNFTDeployerLibrary
} from "../libs/SuperfluidNFTDeployerLibrary.sol";

contract SuperTokenDeployer {

    string public constant RESOLVER_BASE_SUPER_TOKEN_KEY = "supertokens.test.";
    string public constant RESOLVER_BASE_TOKEN_KEY = "tokens.test.";

    SuperTokenFactory internal superTokenFactory;
    TestResolver internal testResolver;

    constructor(address superTokenFactoryAddress, address resolverAddress) {
        // @note SuperfluidFrameworkDeployer must be deployed at this point

        superTokenFactory = SuperTokenFactory(superTokenFactoryAddress);
        testResolver = TestResolver(resolverAddress);
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

        string memory superTokenSymbol = string.concat(_underlyingSymbol, "x");

        superToken = SuperToken(
            address(
                superTokenFactory.createERC20Wrapper(
                    ERC20WithTokenInfo(address(underlyingToken)),
                    ISuperTokenFactory.Upgradability.SEMI_UPGRADABLE,
                    string.concat("Super ", _underlyingSymbol),
                    superTokenSymbol
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
    /// @param _symbol The super token symbol
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
    /// @dev We specify the initial supply (because non-downgradeable) on creation and send it to the deployer
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

        _handleResolverList(
            true,
            string.concat(RESOLVER_BASE_SUPER_TOKEN_KEY, _symbol),
            address(pureSuperToken)
        );

        // transfer initial supply to deployer
        pureSuperToken.transfer(msg.sender, _initialSupply);
    }

    function _handleResolverList(
        bool _listOnResolver,
        string memory _resolverKey,
        address _superTokenAddress
    ) internal {
        if (_listOnResolver) {
            testResolver.set(_resolverKey, address(_superTokenAddress));
        }
    }

    /// @notice Transfer ownership of the TestGovernance contract
    /// @dev This function allows you to transfer ownership of TestGovernance when testing
    /// @param newOwner the new owner of the TestGovernance contract
    function transferOwnership(address newOwner) public {
        TestGovernance testGovernance = TestGovernance(
            testResolver.get("TestGovernance.test")
        );
        testGovernance.transferOwnership(newOwner);
    }
}
