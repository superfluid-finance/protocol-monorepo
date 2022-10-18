// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { CFAv1Forwarder } from "./CFAv1Forwarder.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";

import { Superfluid } from "../superfluid/Superfluid.sol";
import {
    ISuperfluidToken
} from "../interfaces/superfluid/ISuperfluidToken.sol";
import { TestGovernance } from "./TestGovernance.sol";
import {
    ConstantFlowAgreementV1
} from "../agreements/ConstantFlowAgreementV1.sol";
import {
    InstantDistributionAgreementV1
} from "../agreements/InstantDistributionAgreementV1.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper,
    ERC20WithTokenInfo
} from "../superfluid/SuperTokenFactory.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";

import { SETHProxy } from "../tokens/SETH.sol";
import { PureSuperToken } from "../tokens/PureSuperToken.sol";
import {
    IConstantFlowAgreementHook
} from "../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { IPureSuperToken } from "../interfaces/tokens/IPureSuperToken.sol";
import { ISETH } from "../interfaces/tokens/ISETH.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";

import { TestToken } from "./TestToken.sol";

/// @title Superfluid Framework Deployer
/// @notice This is NOT for deploying public nets, but rather only for tesing envs
contract SuperfluidFrameworkDeployer {
    string public constant RESOLVER_BASE_SUPER_TOKEN_KEY = "supertokens.test.";
    string public constant RESOLVER_BASE_TOKEN_KEY = "tokens.test.";

    struct Framework {
        TestGovernance governance;
        Superfluid host;
        ConstantFlowAgreementV1 cfa;
        CFAv1Library.InitData cfaLib;
        InstantDistributionAgreementV1 ida;
        IDAv1Library.InitData idaLib;
        SuperTokenFactory superTokenFactory;
        TestResolver resolver;
        SuperfluidLoader superfluidLoader;
        CFAv1Forwarder cfaV1Forwarder;
    }

    TestGovernance internal governance;
    Superfluid internal host;
    ConstantFlowAgreementV1 internal cfa;
    InstantDistributionAgreementV1 internal ida;
    SuperTokenFactory internal superTokenFactory;
    TestResolver internal resolver;
    SuperfluidLoader internal superfluidLoader;
    CFAv1Forwarder internal cfaV1Forwarder;

    constructor() {
        // @note ERC1820 must be deployed for this to work

        // Deploy TestGovernance. Needs initialization later.
        governance = new TestGovernance();

        // Deploy Superfluid
        host = new Superfluid(true, false);

        // Initialize Superfluid with Governance address
        host.initialize(governance);

        // Initialize Governance
        address[] memory trustedForwarders = new address[](0);
        governance.initialize(
            host,
            address(69),
            4 hours,
            30 minutes,
            trustedForwarders
        );

        // Deploy ConstantFlowAgreementV1
        // TODO @note Once we have the actual implementation for the hook contract,
        // we will need to deploy it and put it here
        cfa = new ConstantFlowAgreementV1(
            host,
            IConstantFlowAgreementHook(address(0)),
            // use 200k as default
            200000
        );

        // Register ConstantFlowAgreementV1 TestGovernance
        governance.registerAgreementClass(host, address(cfa));

        // Deploy CFAv1Forwarder
        cfaV1Forwarder = new CFAv1Forwarder(host);

        // Enable CFAv1Forwarder as a Trusted Forwarder
        governance.enableTrustedForwarder(
            host,
            ISuperfluidToken(address(0)),
            address(cfaV1Forwarder)
        );

        // Deploy InstantDistributionAgreementV1
        ida = new InstantDistributionAgreementV1(host);

        // Register InstantDistributionAgreementV1 with Governance
        governance.registerAgreementClass(host, address(ida));

        // Deploy SuperTokenFactoryHelper
        SuperTokenFactoryHelper superTokenFactoryHelper = new SuperTokenFactoryHelper();

        // Deploy SuperTokenFactory
        superTokenFactory = new SuperTokenFactory(
            host,
            superTokenFactoryHelper
        );

        // 'Update' code with Governance and register SuperTokenFactory with Superfluid
        governance.updateContracts(
            host,
            address(0),
            new address[](0),
            address(superTokenFactory)
        );

        // Deploy Resolver and grant the deployer of SuperfluidFrameworkDeployer admin privileges
        resolver = new TestResolver(msg.sender);

        // Deploy SuperfluidLoader
        superfluidLoader = new SuperfluidLoader(resolver);

        // Register Governance with Resolver
        resolver.set("TestGovernance.test", address(governance));

        // Register Superfluid with Resolver
        resolver.set("Superfluid.test", address(host));

        // Register SuperfluidLoader with Resolver
        resolver.set("SuperfluidLoader-v1", address(superfluidLoader));

        resolver.set("CFAv1Forwarder", address(cfaV1Forwarder));
    }

    /// @notice Fetches the framework contracts
    function getFramework() external view returns (Framework memory sf) {
        sf = Framework({
            governance: governance,
            host: host,
            cfa: cfa,
            cfaLib: CFAv1Library.InitData(host, cfa),
            ida: ida,
            idaLib: IDAv1Library.InitData(host, ida),
            superTokenFactory: superTokenFactory,
            resolver: resolver,
            superfluidLoader: superfluidLoader,
            cfaV1Forwarder: cfaV1Forwarder
        });
        return sf;
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
